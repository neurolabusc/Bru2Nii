
const
kVers = 'Bruker2NIfTI v1.0.20170719 : by Matthew Brett, Andrew Janke, Mikaël Naveau, Chris Rorden.'
{$IFDEF LCLcocoa} + ' Cocoa' {$ENDIF}
{$IFDEF LCLcarbon} + ' Carbon' {$ENDIF}
{$IFDEF LCLGTK2} + ' GTK2' {$ENDIF}
{$IFDEF Darwin} + ' macOS' {$ENDIF}
{$IFDEF Linux} + ' Linux' {$ENDIF}
{$IFDEF WINDOWS}+ ' Windows' {$ENDIF}
{$IFDEF CPU64} + ' 64-bit';
{$ELSE} + ' 32-bit'; {$ENDIF}
 kNIFTI_XFORM_UNKNOWN = 0;
 kNIFTI_XFORM_SCANNER_ANAT = 1;//Scanner-based anatomical coordinates
{$IFDEF ENDIAN_BIG}
 kNIFTI_MAGIC_EMBEDDED_HDR = $6E2B3100;
{$ELSE}
kNIFTI_MAGIC_EMBEDDED_HDR = $00312B6E;
{$ENDIF}

type
  TVec = Record
    v: array [1..9] of single;
  end;
  TVecN = array of single;
  TMat = array[1..4, 1..4] of double;
  TBrukerHdr = Record
    RECO_reco_exists, RECO_byte_order_little, RECO_transposition_varies, VISU_IsDerived, VISU_visu_exists: boolean;
    RECO_transposition, RECO_wordtype_bits, ACQ_ns_list_size, ACQ_nr_completed,
    ACQ_NR, ACQ_NI, ACQ_NSLICES, METHOD_PVM_ObjOrderScheme, ACQ_n_echo_images,
    VISU_Slice_Order, VISU_ExperimentNumber, VISU_ProcessingNumber: integer;
    RECO_map_offset, RECO_map_slope,   ACQ_RG, ACQ_slice_thick,ACQ_read_offset,
    ACQ_phase1_offset, METHOD_slice_offset, METHOD_slice_distance: single;
    METHOD_PVM_SpatResol,RECO_offset, RECO_size, RECO_fov, ACQ_grad_matrix, VISU_orientation,
    ACQ_slice_sepn, ACQ_repetition_time, VISU_CorePosition, VISU_FGOrder : TVec;
    METHOD_SPackArrSliceOrient, ACQ_protocol_name, ACQ_method, VISU_SeriesTypeId, VISU_SubjectPosition : string[79];
    VISU_Scaling, VISU_Intercept: TVecN;
  end;
  TNiHdr= packed record //Next: analyze Format Header structure
    HdrSz : longint; //MUST BE 348
    Data_Type: array [1..10] of ansichar; //unused
    db_name: array [1..18] of ansichar; //unused
    extents: longint; //unused
    session_error: smallint; //unused
    regular: ansichar; ////unused: in Analyze 7.5 this must be 114
    dim_info: byte; //MRI slice order
    dim: array[0..7] of smallint; //Data array dimensions
    intent_p1, intent_p2, intent_p3: single;
    intent_code: smallint;
    datatype: smallint;
    bitpix: smallint;
    slice_start: smallint;
    pixdim: array[0..7]of single;
    vox_offset: single;
    scl_slope: single;//scaling slope
    scl_inter: single;//scaling intercept
    slice_end: smallint;
    slice_code: byte; //e.g. ascending
    xyzt_units: byte; //e.g. mm and sec
    cal_max,cal_min: single; //unused
    slice_duration: single; //time for one slice
    toffset: single; //time axis to shift
    glmax, glmin: longint; //UNUSED
    descrip: array[1..80] of ansichar;
    aux_file: array[1..24] of ansichar;
    qform_code, sform_code: smallint;
    quatern_b,quatern_c,quatern_d,
    qoffset_x,qoffset_y,qoffset_z: single;
    srow_x: array[0..3]of single;
    srow_y: array[0..3]of single;
    srow_z: array[0..3]of single;
    intent_name: array[1..16] of ansichar;
    magic: longint;
    unused: longint; //WARNING: this is not actually part of the NIfTI header, but pads 348-byte header to be 352 bytes (evenly divisible by 8, required for NII files)
end; //TNiHdr Header Structure

function MtZero: TMat;
//4x4 zero matrix, in matlab "zeros(4,4)"
var
  r,c: integer;
begin
  for r := 1 to 4 do
    for c := 1 to 4 do
      result[r,c] := 0;
end;

function MtEye: TMat;
//4x4 identity matrix, in matlab "eye(4)"
var
  i: integer;
begin
  result := MtZero;
  for i := 1 to 4 do
    result[i,i] := 1;
end;

function MtMult (a,b: TMat):  TMat;
//multiply two 4x4 matrices (not commutative: order matters), in matlab "a * b"
var
  i,j,k:  integer;
begin
  result := MtZero;
  for i := 1 to 4 do
    for j := 1 to 4 do
      for k := 1 to 4 do
        result[i,j] := result[i,j]+ a[i,k]*b[k,j];
end;

function MtSet (a,b,c,d, e,f,g,h, i,j,k,l: single):  TMat;
//define a 4x4 matrix,  in Matlab [a,b,c,d; e,f,g,h; i,j,k,l; 0 0 0 1]
begin
  result := MtEye; //set final row to 0,0,0,1
  result[1,1] := a;
  result[1,2] := b;
  result[1,3] := c;
  result[1,4] := d;
  result[2,1] := e;
  result[2,2] := f;
  result[2,3] := g;
  result[2,4] := h;
  result[3,1] := i;
  result[3,2] := j;
  result[3,3] := k;
  result[3,4] := l;
end;

procedure ClearHdr (var lHdr: TNiHdr);
//create empty NIfTI header
var
 lInc: integer;
begin
  with lHdr do begin
    HdrSz := 348;//sizeof(AHdr);
    for lInc := 1 to 10 do
       Data_Type[lInc] := chr(0);
    for lInc := 1 to 18 do
       db_name[lInc] := chr(0);
    extents:=0;
    session_error:= 0;
    regular:='r';
    dim_info:=(0);
    dim[0] := 4;
    for lInc := 1 to 7 do
       dim[lInc] := 0;
    intent_p1 := 0;
    intent_p2 := 0;
    intent_p3 := 0;
    intent_code:=0;
    datatype:=0 ;
    bitpix:=0;
    slice_start:=0;
    for lInc := 1 to 7 do
       pixdim[linc]:= 1.0;
    vox_offset:= 352;
    scl_slope := 1.0;
    scl_inter:= 0.0;
    slice_end:= 0;
    slice_code := 0;
    xyzt_units := 10;
    cal_max:= 0.0;
    cal_min:= 0.0;
    slice_duration:=0;
    toffset:= 0;
    glmax:= 0;
    glmin:= 0;
    for lInc := 1 to 80 do
      descrip[lInc] := chr(0);{80 spaces}
    for lInc := 1 to 24 do
      aux_file[lInc] := chr(0);{24 spaces}
    //below are standard settings which are not 0
    bitpix := 16;//8bits per pixel, e.g. unsigned char
    DataType := 4;//2=unsigned char, 4=16bit
    Dim[0] := 3;
    Dim[1] := 256;
    Dim[2] := 256;
    Dim[3] := 128;
    Dim[4] := 1; {n vols}
    Dim[5] := 1;
    Dim[6] := 1;
    Dim[7] := 1;
    glMin := 0;
    glMax := 255;
    qform_code := kNIFTI_XFORM_UNKNOWN;
    sform_code:= kNIFTI_XFORM_UNKNOWN;
    quatern_b := 0;
    quatern_c := 0;
    quatern_d := 0;
    qoffset_x := 0;
    qoffset_y := 0;
    qoffset_z := 0;
    for lInc := 0 to 3 do
      srow_x[lInc] := 0;
    for lInc := 0 to 3 do
        srow_y[lInc] := 0;
    for lInc := 0 to 3 do
        srow_z[lInc] := 0;
    for lInc := 1 to 16 do
        intent_name[lInc] := chr(0);
    //next: create identity matrix: if code is switched on there will not be a problem
    srow_x[0] := 1;
    srow_y[1] := 1;
    srow_z[2] := 1;
    magic := kNIFTI_MAGIC_EMBEDDED_HDR;//kNIFTI_MAGIC_SEPARATE_HDR;
  end; //with the NIfTI header...
end;

function FSize (lFName: String): longint;
//report size (in bytes) of a file
var SearchRec: TSearchRec;
begin
  FSize := 0;
  if not fileexists(lFName) then exit;
  FindFirst(lFName, faAnyFile, SearchRec);
  FSize := SearchRec.size;
  FindClose(SearchRec);
end;

function UniqueFilename(InStr: string): string;
//ensure we do not overwrite existing file
var
   i: integer;
begin
     result := InStr;
     if not fileexists(result) then
        exit;
     for i := 1 to 9999 do begin
           result := ExtractFilePath  (InStr)+inttostr(i)+ '_'+ExtractFileName  (InStr);
           if not fileexists(result) then
              exit;
     end;
     showmsg('Filename already exists '+InStr);
     result := '';
end;

function BrFindFile (var lxBaseDir,lxPath: string; lxFileName: string; lPrompt: boolean; ACQPdir1, dir2dseq1: string): boolean;
//start from basedir and search sub-folders for a file named filename.
var
   lxSearchDir,lxNewDir, lxTemp : string;
   lxSearchRec: TSearchRec;
begin
     result := false;
     if (dir2dseq1 <> '') and (fileexists(dir2dseq1+lxFileName)) then begin
        lxPath := dir2dseq1+lxFileName;
        result := true;
        exit;
     end;
     lxSearchDir := lxBaseDir;
     lxPath := '';
     while (length(lxSearchDir) > 2) and (lxPath = '') do begin
           if fileexists (lxSearchDir+lxFileName) then
              lxPath := lxSearchDir+ lxFileName
           else begin //search in next outer directory
                repeat
                      delete(lxSearchDir,length(lxSearchDir), 1);
                until (length(lxSearchDir) < 2) or (lxSearchDir[length(lxSearchDir)] = pathdelim);
           end;
     end;
     //did not find file by going to outer directory: no look into subdirectories...
     //if someone started at c:\ this could go on for a long time - so only check first directory.
     if lxPath = '' then begin
        lxSearchDir := lxBaseDir;
        repeat
           lxNewDir := '';
           if FindFirst(lxSearchDir+'*', $00000010 {directories}, lxSearchRec) = 0 then begin
              repeat
                    if (lxSearchRec.Name <> '.') and (lxSearchRec.Name <> '..') and ((lxSearchRec.Attr and faDirectory) <> 0) then begin
                       lxNewDir := lxSearchDir +lxSearchRec.Name+pathdelim;
                    end;
              until (FindNext(lxSearchRec) <> 0) or (lxNewDir <> '');
           end;
           FindClose(lxSearchRec);
           lxSearchDir := lxNewDir; //go one directory deeper into search path
           if fileexists (lxSearchDir+lxFileName) then
              lxPath := lxSearchDir+ lxFileName
        until (lxNewDir = '') {exhausted directories} or (lxPath <> '') {found file};
     end;
     if (lxPath = '') and (ACQPdir1 <> '') then begin //hail mary : e.g. series 2 does not have a reco file, so use series 1. Kludge until Bruker adopts a sensible format
        lxTemp := ACQPdir1 + 'pdata' + pathdelim + '1' + pathdelim + lxFileName;
        if fileexists (lxTemp) then begin
              lxPath := lxTemp;
              showmsg('Warning: Unable to find '+lxFileName+' in '+lxBaseDir+' using '+lxPath );
        end;
     end;
     if (lxPath = '') then begin
        if lPrompt then
           showmsg('BrukerConv error: Unable to find the file named '''+lxFileName+''' in the path '+lxBaseDir)
     end else result := true;
end;

function  BrVecZero: TVec;
//zero a 4-item vector, Matlab zeros(4,1)
var
   i: integer;
begin
  for i := 1 to 9 do
         result.v[i] := 0;
end;

function BrClear : TBrukerHdr;
//reset Bruker format header
begin
     result.RECO_reco_exists := false;
     result.RECO_transposition_varies := false;
     result.ACQ_protocol_name:='';
     result.RECO_byte_order_little := false;
     result.RECO_wordtype_bits := 0;
     result.RECO_transposition := 0;
     result.ACQ_NI := 0;
     result.ACQ_NSLICES := 0;
     result.ACQ_NR := 0;
     result.ACQ_ns_list_size := 0;
     result.ACQ_nr_completed := 0;
     result.ACQ_slice_thick := 0;
     result.ACQ_read_offset := 0;
     result.ACQ_phase1_offset := 0;
     result.METHOD_slice_offset := 0;
     result.METHOD_PVM_ObjOrderScheme:= 0; //slice order unknown
     result.METHOD_slice_distance := 0;
     result.RECO_size:= BrVecZero;
     result.RECO_offset := BrVecZero;
     result.RECO_map_offset:= 0;
     result.RECO_map_slope:= 1;
     result.ACQ_RG:= 1;
     result.RECO_fov := BrVecZero;
     result.ACQ_slice_sepn := BrVecZero;
     result.ACQ_repetition_time := BrVecZero;
     result.ACQ_grad_matrix := BrVecZero;
     result.METHOD_PVM_SpatResol := BrVecZero;
     result.VISU_orientation := BrVecZero;
     result.VISU_CorePosition := BrVecZero;
     result.VISU_FGOrder := BrVecZero;
     result.METHOD_SPackArrSliceOrient := '';
     result.VISU_SeriesTypeId:= '';
     result.VISU_SubjectPosition:= '';
     result.VISU_ExperimentNumber := 0;
     result.VISU_ProcessingNumber := 0;
     result.VISU_IsDerived := false;
     result.VISU_visu_exists := false;
     result.VISU_Slice_Order:= 1;
     setlength(result.VISU_Scaling,0);
     setlength(result.VISU_Intercept,0);
end;

function BrArrayVaries(var lFile: TextFile; var lVaries: boolean): single;
//return first value in array - generate error if different elements of array have different values
// This helps detect if a single Bruker image is composed of orthogonal slices (coronal, sagittal, axial)
const
  delims = [Chr(9), ' '];
var
   str : string;
   prev,i,n: integer;
   v: single;
begin
     lVaries := false;
     result := 0;
     if Eof(lFile) then exit;
     ReadLn(lFile, str);
     prev := 0;
     n := 0;
     for i := 1 to length(str) do begin
         if (str[i] in delims) or (i = length(str))  then begin
             if (i-prev) > 0 then begin
               v := strtofloat(trim(Copy(str, prev + 1, i - prev)));
               n := n + 1;
               if n = 1 then
                  result := v
               else begin
                   if v <> result then
                    lVaries := true;
               end;
            end;
            prev := i;
         end;
     end;
     //if lVaries then
     //   showmsg('WARNING: RECO_transposition varies: unable to convert to NIfTI (localizer scan?) '+lFName);
end;

function TrimStr(lStr: string): string;
//##$VisuExperimentNumber=2 -> "2"
//##$VisuStation=( 65 ) -> "5"
//##$VisuCoreOrientation=( 5, 9 ) -> "5"
var
   i,j: integer;
begin
  result := RightStr(lStr, length(lStr)-AnsiPos('=', lStr));
  i := LastDelimiter('(',result);
  j := LastDelimiter(',',result);
  if j < 1 then
     j := LastDelimiter(')',result);
  if (j < 3) then
     j := length(result)+1;
  result := Copy(result, i+1,j-i-1);
  result := trim(result);
end;

function BrGetProd(s: string): integer;
//'##$VisuCoreOrientation=( 5, 9 ) -> 45
//##$VisuCorePosition=( 1, 3 ) -> 3
//##$PVM_Nucleus1=( 8 ) -> 8
//##$PVM_NAverages=23 -> 23
var
   i,j: integer;
   t: string;
begin
     result := 0;
     i := LastDelimiter('(',s);
     if i < 1 then
        i := LastDelimiter('=',s);
     if i < 1 then
        exit;
     result := 1;
     i := i + 1;
     while i < length(s) do begin
       j := PosEx(',',s,i);
       if j < 1 then
          j := PosEx(')',s,i);
       if j < 1 then
          j := length(s)+1;
       result := result * strtointdef(trim(Copy(s, i,j-i)),1);
       i := j + 1;
     end;
end;

function BrGetArray(var lFile: TextFile;  lStr: string; var lRA: TVec): single;
//read array from Bruker header
var
  i,n: integer;
  f: single;
begin
     result := 0;
     if Eof(lFile) then exit;
     n := BrGetProd(lStr);
     if n < 1 then
      exit;
     //if n > 9 then
     // n := 9; //TVec has only 9 components - for more use BrGetArrayN
     for i := 1 to n do begin
       if i <= 9 then
          Read(lFile, lRA.v[i])
       else
           Read(lFile, f);
     end;
     Readln(lFile);
     result := lRA.v[1];
end;

procedure BrGetArrayN(var lFile: TextFile;  lStr: string; var lRA: TVecN);
var
   n, i: integer;
begin
     setlength(lRA,0); //empty
     if Eof(lFile) then exit;
     n := BrGetProd(lStr);
     if n < 1 then exit;
     setlength(lRA,n);
     //profound
     for i := 0 to (N-1) do
         Read(lFile, lRA[i]);
     readln(lFile);
     //showmessage(inttostr(N));
end;

function BrGetInt (var lStr: string): integer;
//read integer from Bruker header
begin
  result := StrToInt(TrimStr(lStr));
  //result := StrToInt(RightStr(lStr, length(lStr)-AnsiPos('=', lStr)));
end;

function BrGetFloat (var lStr: string): single;
//read float from Bruker header
begin
  result := StrToFloat(TrimStr(lStr));
  //result := StrToFloat(RightStr(lStr, length(lStr)-AnsiPos('=', lStr)));
end;

function BrGetString (var lFile: TextFile): string;
//extract string from Bruker header file, e.g. "<T1 localizer>" becomes "T1_localizer"
const
  InvalidChars : set of char =
    [',','.','/','!','@','#','$','%','^','&','*','''','"',';','_','(',')',':','|','[',']'];
var
   c: char;
begin
     result := '';
     if Eof(lFile) then exit;
     ReadLn(lFile, result);
     result  := StringReplace(result, '<', '',  [rfReplaceAll, rfIgnoreCase]);
     result  := StringReplace(result, '>', '',  [rfReplaceAll, rfIgnoreCase]);
     //next: remove symbols that are incompatible or confusing with a filename
     for c in InvalidChars do
         result  := StringReplace(result, c, '_',  [rfReplaceAll, rfIgnoreCase])
end;

function Slice_Order(lOrderStr: string; var lRA: TVec): integer;
//is "FG_SLICE" 1st (default), 2nd or Nth item:
//(22, <FG_DTI>, <DTI>, 0, 3) (64, <FG_SLICE>, <>, 3, 2)  -> 2 [22, 64]
//(6, <FG_MOVIE>, <diffusion>, 0, 1)  -> 1 [6]
//(5, <FG_SLICE>, <>, 0, 2) (9, <FG_DIFFUSION>, <diffusion>, 2, 2) (3,<FG_CYCLE>, <repetitions>, 4, 1) // -> 1 [5,9,3]
//(2, <FG_ECHO>, <>, 0, 1) (9, <FG_SLICE>, <>, 1, 2) (2, <FG_CYCLE>, <>, 3, 0) // -> 2 [2,9,2]
const
  kDelimiter = ')';
var
  n,i,j,offset: integer;
begin
  //read array: first number after '('
     i := Pos('(',lOrderStr);
     n := 0;
     while (i > 0)  do begin
       j := PosEx(',',lOrderStr,i);
       if (j > 0) and (n < 10) then begin
          inc(n);
          //showmessage(trim(Copy(lOrderStr, i+1,j-i-1)));
          lRA.v[n] := strtointdef(trim(Copy(lOrderStr, i+1,j-i-1)),1);
       end;
       i := PosEx('(',lOrderStr,j);
     end;
     //read position of FG_SLICE
     result := 1;
     offset := AnsiPos('<FG_SLICE>', lOrderStr);
     if offset < 2 then exit; //slice not reported, e.g. "(6, <FG_MOVIE>, <diffusion>, 0, 1)"
     lOrderStr := Copy(lOrderStr, 1, offset);
     offset := PosEx(kDelimiter, lOrderStr, 1);
     while offset <> 0 do begin
           inc(result);
           offset := PosEx(kDelimiter, lOrderStr, offset + length(kDelimiter));
     end;
end;

function BrReadVISU_PARS (fname: string; var BrHdr: TBrukerHdr): boolean;
//read Bruker format visu_pars file
var
   unusedVec: TVec;
   lFile: TextFile;
   lStr :string;
begin
  result := false;
  if (fname = '') or (not fileexists(fname)) then exit;
  assignfile(lFile,fname);
  reset(lFile);
  BrHdr.VISU_visu_exists := true;
  while not Eof(lFile) do begin
        ReadLn(lFile, lStr);
        //properties also in ACQP file
        if AnsiPos('##$VisuAcquisitionProtocol=', lStr) = 1 then
           BrHdr.ACQ_protocol_name :=  BrGetString(lFile);
        if AnsiPos('##$VisuAcqSequenceName=', lStr) = 1 then
           BrHdr.ACQ_method := BrGetString(lFile);
        if AnsiPos('##$VisuCoreOrientation=', lStr) = 1 then begin
           BrHdr.ACQ_NI := BrGetInt(lStr);    //5 for ##$VisuCoreOrientation=( 5, 9 )
           BrHdr.ACQ_NSLICES := BrHdr.ACQ_NI; //not sure how multiple echo images look?
           BrGetArray(lFile, '(9)', BrHdr.VISU_orientation);
        end;
        if AnsiPos('##$VisuCoreFrameThickness=', lStr) = 1 then //= ACQ_slice_thick: slice thickness without gap
           BrHdr.ACQ_slice_thick := BrGetArray(lFile, lStr, unusedVec); //See Matthew Brett's Banana example (1.5mm slice + 0.5mm gap = 2mm)
        //TO DO : multiple echoes, slice gap
        (*if AnsiPos('##$VisuCoreSlicePacksSliceDist', lStr) = 1 then begin  //T2 rare lists as 0mm?
           BrHdr.METHOD_slice_distance := BrGetArray(lFile, '(1)',unusedVec );
           BrHdr.ACQ_slice_thick := BrHdr.METHOD_slice_distance;
        end; *)
        if AnsiPos('##$VisuAcqRepetitionTime=', lStr) = 1 then
           BrGetArray(lFile, lStr, BrHdr.ACQ_repetition_time);
        if AnsiPos('##$VisuAcqEchoTime=', lStr) = 1 then
           BrHdr.ACQ_n_echo_images := BrGetInt(lStr);
        //properties also in RECO file
        if AnsiPos('##$VisuCoreByteOrder=littleEndian', lStr) = 1 then
           BrHdr.RECO_byte_order_little:= true; //
        if AnsiPos('##$VisuCoreWordType=_16BIT_SGN_INT', lStr) = 1 then //
           BrHdr.RECO_wordtype_bits := 16;
        if AnsiPos('##$VisuCoreWordType=_32BIT_SGN_INT', lStr) = 1 then //
           BrHdr.RECO_wordtype_bits := 32;
        //for some VISU files VisuCoreSize is XY in others it is YX
        // it is unclear when we should choose one or the other representation
        // therefore, we will keep the RECO or METHOD (##$PVM_Matrix) if it is available, as it
        // does not exhibit this issue.
        if (AnsiPos('##$VisuCoreSize=', lStr) = 1) and (BrHdr.RECO_size.v[1] = 0) then begin
        //if (AnsiPos('##$VisuCoreSize=', lStr) = 1) then begin
           BrGetArray(lFile, lStr, BrHdr.RECO_size);
           if (BrHdr.RECO_size.v[1] <> BrHdr.RECO_size.v[2]) then
              showmsg('Warning: number of rows/columns may be swapped (relying on visu_pars)');
        end;
        if AnsiPos('##$VisuCoreExtent=', lStr) = 1 then begin
           BrGetArray(lFile, lStr, BrHdr.RECO_fov);
           BrHdr.RECO_fov.v[1] := BrHdr.RECO_fov.v[1] * 0.1;
           BrHdr.RECO_fov.v[2] := BrHdr.RECO_fov.v[2] * 0.1;
        end;
        //properties only in VISU
        if AnsiPos('##$VisuExperimentNumber=', lStr) = 1 then
          BrHdr.VISU_ExperimentNumber := BrGetInt(lStr);
        if AnsiPos('##$VisuProcessingNumber=', lStr) = 1 then
           BrHdr.VISU_ProcessingNumber := BrGetInt(lStr);
        if AnsiPos('##$VisuSubjectPosition=', lStr) = 1 then begin
           BrHdr.VISU_SubjectPosition := trim(RightStr(lStr, length(lStr)-AnsiPos('=', lStr)));
        end;
        if AnsiPos('##$VisuSeriesTypeId', lStr) = 1 then begin
           BrHdr.VISU_SeriesTypeId :=  BrGetString(lFile);
           //SeriesTypeId begins with "ACQ_" (e.g. "ACQ_BRUKER_PVM") for raw data and "DERIVED_" (e.g. "DERIVED_ISA") for derived data
           if AnsiPos('DERIVED_', BrHdr.VISU_SeriesTypeId) = 1 then
             BrHdr.VISU_IsDerived := true;
        end;
        (*if AnsiPos('##$VisuAcqImagePhaseEncDir=', lStr) = 1 then begin
          ReadLn(lFile, lStr);
          if (lstr = 'row_slice_dir') then BrHdr.VISU_FlipXY:=true;
        end;*)

        if AnsiPos('##$VisuCorePosition', lStr) = 1 then
           BrGetArray(lFile, lStr, BrHdr.VISU_CorePosition);  //VISU_CorePosition
        if AnsiPos('##$VisuCoreWordType=_32BIT_SGN_INT', lStr) = 1 then
           BrHdr.RECO_wordtype_bits := 32; //for derived images, VISU overwrites RECO
        if AnsiPos('##$VisuCoreWordType=_16BIT_SGN_INT', lStr) = 1 then
           BrHdr.RECO_wordtype_bits := 16; //for derived images, VISU overwrites RECO
        if AnsiPos('##$VisuFGOrderDesc=', lStr) = 1 then begin
          ReadLn(lFile, lStr);
          BrHdr.VISU_Slice_Order := Slice_Order(lStr,BrHdr.VISU_FGOrder);
        end;
        if AnsiPos('##$VisuCoreDataSlope=', lStr) = 1 then //##$VisuCoreDataSlope=( 330 ) 1e-06 1e-09 1.34593219873039e-07 ....
	   BrGetArrayN(lFile, lStr, BrHdr.VISU_Scaling);
	if AnsiPos('##$VisuCoreDataOffs=', lStr) = 1 then //
	   BrGetArrayN(lFile, lStr, BrHdr.VISU_Intercept);
  end;
  //we can derive mm resolution from FOV and voxel resolution
  if (BrHdr.RECO_size.v[1] > 0) then
     BrHdr.METHOD_PVM_SpatResol.v[1] := BrHdr.RECO_fov.v[1]/BrHdr.RECO_size.v[1];
  if (BrHdr.RECO_size.v[2] > 0) then
     BrHdr.METHOD_PVM_SpatResol.v[2] := BrHdr.RECO_fov.v[1]/BrHdr.RECO_size.v[2];
  closefile(lFile);
  result := true;
  //result := false;
end;

function BrReadMETHOD (fname: string; var BrHdr: TBrukerHdr): boolean;
//read Bruker format Method file
var
   lFile: TextFile;
   lStr :string;
   lUnused: TVec;
begin
  result := false;
  if (fname = '') or (not fileexists(fname)) then exit;
  assignfile(lFile,fname);
  reset(lFile);
  while not Eof(lFile) do begin
        ReadLn(lFile, lStr);
        if AnsiPos('##$PVM_SPackArrSliceOrient=', lStr) = 1 then
           BrHdr.METHOD_SPackArrSliceOrient :=  BrGetString(lFile);
        if AnsiPos('##$PVM_SPackArrSliceOffset', lStr) = 1 then
           BrHdr.METHOD_slice_offset := BrGetArray(lFile, '(1)',lUnused );
        if AnsiPos('##$PVM_SPackArrSliceDistance', lStr) = 1 then
           BrHdr.METHOD_slice_distance := BrGetArray(lFile, '(1)',lUnused );
        if AnsiPos('##$PVM_ObjOrderScheme', lStr) = 1 then  begin  //4/2016 Suggested and validated by Chern-Chyi Yen
           if AnsiPos('Sequential', lStr) <> 0 then BrHdr.METHOD_PVM_ObjOrderScheme  := 1;
           if AnsiPos('Reverse_sequential', lStr) <> 0 then BrHdr.METHOD_PVM_ObjOrderScheme  := 2;
           if AnsiPos('Interlaced', lStr) <> 0 then BrHdr.METHOD_PVM_ObjOrderScheme  := 3;
           if AnsiPos('Reverse_interlaced', lStr) <> 0 then BrHdr.METHOD_PVM_ObjOrderScheme  := 4;
        end;
        if AnsiPos('##$PVM_SpatReso', lStr) = 1 then
           BrGetArray(lFile, lStr, BrHdr.METHOD_PVM_SpatResol);
        //
        if (AnsiPos('##$PVM_Matrix=', lStr) = 1) then
           BrGetArray(lFile, lStr, BrHdr.RECO_size);
  end;
  result := true;
  closefile(lFile);
end;

function BrReadACQP (fname: string; var BrHdr: TBrukerHdr): boolean;
//read Bruker format acqp file
var
    lFile: TextFile;
    lStr :string;
    lUnused: TVec;
begin
  result := false;
  if (fname = '') or (not fileexists(fname)) then exit;
  assignfile(lFile,fname);
  reset(lFile);
  while not Eof(lFile) do begin
        ReadLn(lFile, lStr);
        if AnsiPos('##$', lStr) <> 1 then continue;
        if AnsiPos('##$ACQ_protocol_name=', lStr) = 1 then
           BrHdr.ACQ_protocol_name :=  BrGetString(lFile);
        if AnsiPos('##$ACQ_slice_sepn=', lStr) = 1 then
           BrGetArray(lFile, lStr, BrHdr.ACQ_slice_sepn);
        if AnsiPos('##$ACQ_grad_matrix=', lStr) = 1 then
           BrGetArray(lFile, '(9)', BrHdr.ACQ_grad_matrix);
        if AnsiPos('##$ACQ_read_offset=', lStr) = 1 then
           BrHdr.ACQ_read_offset := BrGetArray(lFile, '(1)',lUnused );
        if AnsiPos('##$ACQ_phase1_offset=', lStr) = 1 then
           BrHdr.ACQ_phase1_offset := BrGetArray(lFile, '(1)',lUnused );
        if AnsiPos('##$ACQ_repetition_time=', lStr) = 1 then
           BrGetArray(lFile, lStr, BrHdr.ACQ_repetition_time);
        if AnsiPos('##$ACQ_slice_thick', lStr) = 1 then
           BrHdr.ACQ_slice_thick := BrGetFloat(lStr);
        if AnsiPos('##$ACQ_ns_list_size=', lStr) = 1 then
           BrHdr.ACQ_ns_list_size := BrGetInt(lStr);
        if AnsiPos('##$NI=', lStr) = 1 then
           BrHdr.ACQ_NI := BrGetInt(lStr);
        if AnsiPos('##$ACQ_NSLICES=', lStr) = 1 then //##$ACQ_NSLICES=64
           BrHdr.ACQ_NSLICES := BrGetInt(lStr);
        if AnsiPos('##$NR=', lStr) = 1 then
           BrHdr.ACQ_NR := BrGetInt(lStr);
        if AnsiPos('##$ACQ_nr_completed', lStr) = 1 then
           BrHdr.ACQ_nr_completed := BrGetInt(lStr);
        if AnsiPos('##$RG=', lStr) = 1 then
           BrHdr.ACQ_RG := BrGetFloat(lStr);
        if AnsiPos('##$ACQ_method=', lStr) = 1 then
           BrHdr.ACQ_method := BrGetString(lFile);
        if AnsiPos('##$ACQ_n_echo_images', lStr) = 1 then
           BrHdr.ACQ_n_echo_images := BrGetInt(lStr);
  end;
  result := true;
  //if (BrHdr.ACQ_slice_thick <> BrHdr.ACQ_slice_sepn.v[1])then
  //   showmsg(format('%g %g %s',[BrHdr.ACQ_slice_thick, BrHdr.ACQ_slice_sepn.v[1],fname]));
  closefile(lFile);
end;

function BrReadRECO (fname: string; var BrHdr: TBrukerHdr): boolean;
//Read Bruker format RECO file
var
    lFile: TextFile;
    lStr :string;
    RECO_map_offset_varies, RECO_map_slope_varies: boolean;
begin
  result := false;
  if (fname = '') or (not fileexists(fname)) then exit;
  assignfile(lFile,fname);
  reset(lFile);
  BrHdr.RECO_wordtype_bits := 32; //default
  BrHdr.RECO_byte_order_little:= false; //default
  BrHdr.RECO_reco_exists := true;
  while not Eof(lFile) do begin
        ReadLn(lFile, lStr);
        if AnsiPos('##$', lStr) <> 1 then continue;
        if AnsiPos('##$RECO_wordtype=_16BIT_SGN_INT', lStr) = 1 then
           BrHdr.RECO_wordtype_bits := 16;
        if AnsiPos('##$RECO_transposition=', lStr) = 1 then
           BrHdr.RECO_transposition  := round(BrArrayVaries(lFile, BrHdr.RECO_transposition_varies ));
        if AnsiPos('##$RECO_byte_order=littleEndian', lStr) = 1 then
           BrHdr.RECO_byte_order_little:= true;
        if AnsiPos('##$RECO_size=', lStr) = 1 then
           BrGetArray(lFile, lStr, BrHdr.RECO_size);
        if AnsiPos('RECO_offset=', lStr) = 1 then
           BrGetArray(lFile, '(3)', BrHdr.RECO_offset);
        if AnsiPos('##$RECO_fov=', lStr) = 1 then
           BrGetArray(lFile, lStr, BrHdr.RECO_fov);
        if AnsiPos('##$RECO_map_offset=', lStr) = 1 then
           BrHdr.RECO_map_offset  := BrArrayVaries(lFile, RECO_map_offset_varies );
        if AnsiPos('##$RECO_map_slope=', lStr) = 1 then
           BrHdr.RECO_map_slope  := BrArrayVaries(lFile, RECO_map_slope_varies );
  end;
  result := true;
  closefile(lFile);
end;

function f2str (f: single): string;
//convert float to string
begin
     result := FloatToStrF(f, ffGeneral, 8, 4);
end;

procedure BrReport(BrHdr: TBrukerHdr; InName, OutName: string; Verbose: boolean);
//provide text details of Bruker Header
begin
     showmsg('Creating NIfTI: '+OutName);
     if not Verbose then exit;
     showmsg('Bruker Source: '+InName);
     showmsg('ACQ_protocol_name : ' + BrHdr.ACQ_protocol_name);
     showmsg('ACQ_method : ' + BrHdr.ACQ_method);
     if (BrHdr.VISU_SeriesTypeId <> '') then
        showmsg('VISU_SeriesTypeId : ' + BrHdr.VISU_SeriesTypeId);
     if (BrHdr.VISU_SubjectPosition <> '') then
        showmsg('VISU_SubjectPosition : ' + BrHdr.VISU_SubjectPosition );
     if BrHdr.VISU_isDerived then
        showmsg('VISU derived data : true');
     if BrHdr.RECO_byte_order_little then
        showmsg(' Endian: Little')
     else
          showmsg(' Endian: Big');
     showmsg(' Bits : '+ inttostr(BrHdr.RECO_wordtype_bits));
     showmsg(' Size : '+f2str(BrHdr.RECO_size.v[1])+'x'+f2str(BrHdr.RECO_size.v[2])) ;
     showmsg(' FOV : '+f2str(BrHdr.RECO_fov.v[1])+'x'+f2str(BrHdr.RECO_fov.v[2]));
     showmsg(' Matrix : ['+f2str(BrHdr.ACQ_grad_matrix.v[1])+' '+f2str(BrHdr.ACQ_grad_matrix.v[2])+' '+f2str(BrHdr.ACQ_grad_matrix.v[3])+'; '+
      f2str(BrHdr.ACQ_grad_matrix.v[4])+' '+f2str(BrHdr.ACQ_grad_matrix.v[5])+' '+f2str(BrHdr.ACQ_grad_matrix.v[6])+'; '+
      f2str(BrHdr.ACQ_grad_matrix.v[7])+' '+f2str(BrHdr.ACQ_grad_matrix.v[8])+' '+f2str(BrHdr.ACQ_grad_matrix.v[9])+']');
     showmsg(' Sepn (Thick+Gap) : '+f2str(BrHdr.ACQ_slice_sepn.v[1]));
     showmsg(' Slice Thick : '+f2str(BrHdr.ACQ_slice_thick));
     showmsg(' Offset (Read/Phase/Slice) : '+f2str(BrHdr.ACQ_read_offset)+' '+f2str(BrHdr.ACQ_phase1_offset)+' '+f2str(BrHdr.METHOD_slice_offset));
     showmsg(' METHOD_PVM_SpatResol : '+  f2str(BrHdr.METHOD_PVM_SpatResol.v[1])+'x' +f2str(BrHdr.METHOD_PVM_SpatResol.v[2])+'x' +f2str(BrHdr.METHOD_PVM_SpatResol.v[3]));
     showmsg(' METHOD_slice_distance : '+   f2str(BrHdr.METHOD_slice_distance));
     showmsg(' METHOD_PVM_ObjOrderScheme: '+inttostr(BrHdr.METHOD_PVM_ObjOrderScheme));
     showmsg(' RECO_offset : '+f2str(BrHdr.RECO_offset.v[1])+' '+f2str(BrHdr.RECO_offset.v[2])+' '+f2str(BrHdr.RECO_offset.v[3]));
     showmsg(' Repetition Time : '+f2str(BrHdr.ACQ_repetition_time.v[1]) );
     showmsg(' NI (images per TR, slices) : '+inttostr(BrHdr.ACQ_NI));
     showmsg(' NSLICES : '+inttostr(BrHdr.ACQ_NSLICES));
     showmsg(' NR (volumes) : '+inttostr(BrHdr.ACQ_NR));
     showmsg(' Convert pixel value (PV) to floating point value (FP) : FP = (PV/RECO_map_slope + RECO_map_offset)/ ACQ_RG');
     showmsg(' RECO_map_slope : '+   f2str(BrHdr.RECO_map_slope));
     showmsg(' RECO_map_offset : '+   f2str(BrHdr.RECO_map_offset));
     showmsg(' ACQ_RG : '+   f2str(BrHdr.ACQ_RG));
     showmsg(' Transpose : '+f2str(BrHdr.RECO_transposition) );
end;

procedure SwapDims (var i1, i2: smallint; var f1, f2: single);
//Bruker refers to X/Y dimensions in reverse order to NIfTI
var
    i: integer;
    f: single;
begin
  i := i1;
  i1 := i2;
  i2 := i;
  f := f1;
  f1 := f2;
  f2 := f;
end;

procedure MtReport (title: string; mt: TMat);
//display a 4x4 matrix
begin
  showmsg(title+' = ['+f2str(mt[1,1])+' '+f2str(mt[1,2])+' '+f2str(mt[1,3])+' '+f2str(mt[1,4])+'; '
    +f2str(mt[2,1])+' '+f2str(mt[2,2])+' '+f2str(mt[2,3])+' '+f2str(mt[2,4])+'; '
    +f2str(mt[3,1])+' '+f2str(mt[3,2])+' '+f2str(mt[3,3])+' '+f2str(mt[3,4])+'; 0, 0, 0, 1] ');
end;


procedure BrMat_VISU (BrHdr: TBrukerHdr; var NiHdr: TNiHdr; lFOVx10, lVerbose: boolean);
var
    m: mat44;
    vc_orientation: mat33;
    dumdx, dumdy, dumdz: single;
    vc_position, resolution: vect3;
    vc_subject_position: string;
    frame_body_as_frame_head,keep_same_det,consider_subject_position: boolean;
begin
    vc_orientation := toMatrix (BrHdr.VISU_orientation.v[1],BrHdr.VISU_orientation.v[2],BrHdr.VISU_orientation.v[3],
                   BrHdr.VISU_orientation.v[4],BrHdr.VISU_orientation.v[5],BrHdr.VISU_orientation.v[6],
                   BrHdr.VISU_orientation.v[7],BrHdr.VISU_orientation.v[8],BrHdr.VISU_orientation.v[9]);
    vc_position := Vector3(BrHdr.VISU_CorePosition.v[1], BrHdr.VISU_CorePosition.v[2], BrHdr.VISU_CorePosition.v[3]);
    if lFOVx10 then begin
      NiHdr.pixdim[1] := NiHdr.pixdim[1] * 10;
      NiHdr.pixdim[2] := NiHdr.pixdim[2] * 10;
      NiHdr.pixdim[3] := NiHdr.pixdim[3] * 10;
    end;
    resolution := Vector3(NiHdr.pixdim[1], NiHdr.pixdim[2], NiHdr.pixdim[3]);
    vc_subject_position := BrHdr.VISU_SubjectPosition;
    frame_body_as_frame_head :=False;
    keep_same_det := True;
    consider_subject_position :=False;
    m := compute_affine_from_visu_pars(vc_orientation, vc_position, vc_subject_position, resolution,
      frame_body_as_frame_head, keep_same_det,consider_subject_position);

    NiHdr.sform_code:= kNIFTI_XFORM_SCANNER_ANAT;
    NiHdr.srow_x[0] := m[0,0]; NiHdr.srow_x[1] := m[0,1]; NiHdr.srow_x[2] := m[0,2]; NiHdr.srow_x[3] := m[0,3];
    NiHdr.srow_y[0] := m[1,0]; NiHdr.srow_y[1] := m[1,1]; NiHdr.srow_y[2] := m[1,2]; NiHdr.srow_y[3] := m[1,3];
    NiHdr.srow_z[0] := m[2,0]; NiHdr.srow_z[1] := m[2,1]; NiHdr.srow_z[2] := m[2,2]; NiHdr.srow_z[3] := m[2,3];
    NiHdr.qform_code:= kNIFTI_XFORM_SCANNER_ANAT;
    nifti_mat44_to_quatern(m, NiHdr.quatern_b, NiHdr.quatern_c, NiHdr.quatern_d,
                              NiHdr.qoffset_x, NiHdr.qoffset_y, NiHdr.qoffset_z,
                              dumdx, dumdy, dumdz,NiHdr.pixdim[0]) ;
    if not lverbose then exit;
    showmsg(Mat2Str('vc_orientation',vc_orientation));
    showmsg(Vec2Str('vc_position',vc_position));
    showmsg(Vec2Str('resolution',resolution));
    showmsg('vc_subject_position = '+vc_subject_position);
    showmsg(Bool2Str('frame_body_as_frame_head',frame_body_as_frame_head));
    showmsg(Bool2Str('keep_same_det',keep_same_det));
    showmsg(Bool2Str('consider_subject_position',consider_subject_position));
    showmsg(Mat2Str('result',m));
end;

procedure BrMat (BrHdr: TBrukerHdr; var NiHdr: TNiHdr; lFOVx10, lVerbose: boolean);
//Generate NIfTI spatial transformation matrix from Bruker Header
var
  rotn, Tranposition,Translations, swapxy, ft2mm, recoff, perl2nii, so_far: TMat;
begin
  Tranposition := MtEye;
     if (BrHdr.RECO_transposition = 1) then begin
      SwapDims(NiHdr.dim[1],NiHdr.dim[2], NiHdr.pixdim[1],NiHdr.pixdim[2]);
      Tranposition := MtSet(0,1,0,0, 1,0,0,0, 0,0,1,0)   //swap read-phase
     end
     else if (BrHdr.RECO_transposition = 2) then
       Tranposition := MtSet(1,0,0,0, 0,0,1,0, 0,1,0,0) //swap phase-slice
     else if (BrHdr.RECO_transposition = 3) then
       Tranposition := MtSet(0,0,1,0, 0,1,0,0, 1,0,0,0) //swap read-slice
     else if (BrHdr.RECO_transposition <> 0) then begin
       showmsg('Need to handle RECO transposition');
     end;
  swapxy := MtEye;
  swapxy[1,1] := -1;
  swapxy[2,2] := -1;
  //recon offset
  recoff := MtEye;
  recoff[1,4] := BrHdr.RECO_offset.v[1]; //Read
  recoff[2,4] := BrHdr.RECO_offset.v[2]; //Phase
  recoff[3,4] := BrHdr.RECO_offset.v[3]; //Slice
  //voxels ->mm
  ft2mm := MtEye;
  ft2mm[1,1] := NiHdr.pixdim[1];
  ft2mm[2,2] := NiHdr.pixdim[2];
  ft2mm[3,3] := NiHdr.pixdim[3];
  //set origin to center of volume
  ft2mm[1,4] := (NiHdr.dim[1]+1)/2 * -NiHdr.pixdim[1];
  ft2mm[2,4] := (NiHdr.dim[2]+1)/2 * -NiHdr.pixdim[2];
  ft2mm[3,4] := (NiHdr.dim[3]+1)/2 * -NiHdr.pixdim[3];

  //translations
  Translations := MtEye;
  Translations[1,4] := BrHdr.ACQ_read_offset; //Read
  Translations[2,4] := BrHdr.ACQ_phase1_offset; //Phase
  Translations[3,4] := BrHdr.METHOD_slice_offset; //Slice
  //gradients matrix - transposed!
    rotn := MtSet(BrHdr.ACQ_grad_matrix.v[1],BrHdr.ACQ_grad_matrix.v[4],BrHdr.ACQ_grad_matrix.v[7],0,
    BrHdr.ACQ_grad_matrix.v[2],BrHdr.ACQ_grad_matrix.v[5],BrHdr.ACQ_grad_matrix.v[8],0,
    BrHdr.ACQ_grad_matrix.v[3],BrHdr.ACQ_grad_matrix.v[6],BrHdr.ACQ_grad_matrix.v[9],0);
  so_far := MtMult(recoff, Tranposition);
  so_far := MtMult(ft2mm, so_far);
  so_far := MtMult(swapxy, so_far);
  so_far := MtMult(Translations, so_far);
  so_far := MtMult(rotn,so_far);
  //The perl2nii transforms spatial coordinates to NIfTI convention
  // pvconv.pl matrix dimensions X,Y,Z is L->R,I->S,P->A, but NIfTI is explicitly L->R,P->A,I->S
  if lFOVx10 then begin
   perl2nii :=  MtSet(-10,0,0,0, 0,0,10,0, 0,10,0,0);
   NiHdr.pixdim[1] := NiHdr.pixdim[1] * 10;
   NiHdr.pixdim[2] := NiHdr.pixdim[2] * 10;
   NiHdr.pixdim[3] := NiHdr.pixdim[3] * 10;
  end else
      perl2nii :=  MtSet(-1,0,0,0, 0,0,1,0, 0,1,0,0);
  so_far := MtMult(perl2nii,so_far);
  NiHdr.sform_code:= kNIFTI_XFORM_SCANNER_ANAT;
  NiHdr.srow_x[0] := so_far[1,1]; NiHdr.srow_x[1] := so_far[1,2]; NiHdr.srow_x[2] := so_far[1,3]; NiHdr.srow_x[3] := so_far[1,4];
  NiHdr.srow_y[0] := so_far[2,1]; NiHdr.srow_y[1] := so_far[2,2]; NiHdr.srow_y[2] := so_far[2,3]; NiHdr.srow_y[3] := so_far[2,4];
  NiHdr.srow_z[0] := so_far[3,1]; NiHdr.srow_z[1] := so_far[3,2]; NiHdr.srow_z[2] := so_far[3,3]; NiHdr.srow_z[3] := so_far[3,4];
  if not lVerbose then exit; //display transformation values
  MtReport('start',Tranposition);
  MtReport('recoff',recoff);
  MtReport('ft2mm',ft2mm);
  MtReport('swaps',swapxy);
  MtReport('trans',Translations);
  MtReport('rotn',rotn);
  MtReport('perl2nii',perl2nii);
  showmsg('vx2mm =  rotn * trans * swaps * ft2mm * recoff * start');
  showmsg('nii =  perl2nii * rotn * trans * swaps * ft2mm * recoff * start');
  MtReport('NIfTI',so_far);
end;

function BrHdr2NII (BrHdr: TBrukerHdr; var NiHdr: TNiHdr; ImgBytes: integer; var FName: string): boolean;
//convert Bruker Header format to NIfTI format
var
    i: integer;
    str: string;
begin
  result := false;
  ClearHdr (NiHdr);
  if BrHdr.RECO_wordtype_bits = 16 then begin
     NiHdr.datatype := 4;
     NiHdr.bitpix  := 16;
  end else if BrHdr.RECO_wordtype_bits = 32 then begin
      NiHdr.datatype := 8;
      NiHdr.bitpix := 32;
  end else begin
      showmsg('Unsupported RECO_wordtype '+inttostr(BrHdr.RECO_wordtype_bits)+' '+FName);
      exit;
  end;
  NiHdr.scl_inter:= BrHdr.RECO_map_offset/BrHdr.ACQ_RG;  //FP = (PV/RECO_map_slope + RECO_map_offset)/ ACQ_RG
  NiHdr.scl_slope:= (1/BrHdr.RECO_map_slope)/BrHdr.ACQ_RG;  //FP = (PV/RECO_map_slope + RECO_map_offset)/ ACQ_RG
  for i := 1 to 4 do
      NiHdr.dim[i] := round(BrHdr.RECO_size.v[i]);
  if (NiHdr.dim[1] < 1) then begin
     showmsg('Error: first dimension of RECO_size must be non-zero'+FName);
     exit;
  end;
  if (NiHdr.dim[2] < 1) then NiHdr.dim[2]  := 1;
  (*if true then begin //if (BrHdr.VISU_FlipXY) and (NiHdr.dim[2] > 1) then begin //Bruker report YX order [Row,Column] while NIfTI XY [Column,Row]
     showmsg('swapping rows/columns');
     //Only an issue for images where inplane resolution is different for rows and columns
     //swizzle row/column
     i := NiHdr.dim[2];
     NiHdr.dim[2] := NiHdr.dim[1];
     NiHdr.dim[1] := i;
  end;*)

  if (NiHdr.dim[3] < 1) then NiHdr.dim[3]  := 1;
  if (NiHdr.dim[4] < 1) then NiHdr.dim[4]  := 1;
  for i := 1 to 2 do begin
        if BrHdr.RECO_size.v[i] = 0 then begin
          NiHdr.pixdim[i] := 1;
          showmsg('Warning: FOV does not describe all dimensions (localizer scan?) '+FName);
        end else
            NiHdr.pixdim[i] := (10*BrHdr.RECO_fov.v[i])/BrHdr.RECO_size.v[i] ;   //x10 FOV is CM not MM
  end;
  (*if (BrHdr.METHOD_PVM_SpatResol.v[3] <> BrHdr.METHOD_slice_distance) or
    (BrHdr.METHOD_PVM_SpatResol.v[3] <> BrHdr.ACQ_slice_sepn.v[1]) or
    (BrHdr.METHOD_PVM_SpatResol.v[3] <> BrHdr.ACQ_slice_thick) then begin
      showmessage(format('%s %g %g %g %g!',[FName,BrHdr.METHOD_PVM_SpatResol.v[3],BrHdr.METHOD_slice_distance,
        BrHdr.ACQ_slice_sepn.v[1], BrHdr.ACQ_slice_thick ]));
    end;*)
  if BrHdr.METHOD_PVM_SpatResol.v[3] > 0 then //4/2016: Suggested and validated by Chern-Chyi Yen
    NiHdr.pixdim[3] := BrHdr.METHOD_PVM_SpatResol.v[3] //for 3D sequences, e.g. 3D FLASH
  else if BrHdr.METHOD_slice_distance > 0 then
    NiHdr.pixdim[3] := BrHdr.METHOD_slice_distance //best source for distance between slices : includes gap
  else if BrHdr.RECO_size.v[3]> 0 then
    NiHdr.pixdim[3] := (10*BrHdr.RECO_fov.v[3])/BrHdr.RECO_size.v[3] //distance between slices, x10 FOV is CM not MM
  else if BrHdr.ACQ_slice_sepn.v[1] > 0 then
    NiHdr.pixdim[3] := BrHdr.ACQ_slice_sepn.v[1]
  else if BrHdr.ACQ_slice_thick > 0 then begin
       showmsg('Warning: unable to determine distance between slices, using slice thickness (incorrect if gap between slices)');
       NiHdr.pixdim[3] := BrHdr.ACQ_slice_thick; //BrHdr.ACQ_slice_sepn.v[1]; //distance between slices
  end else begin
      showmsg('Error: Unable to determine distance between slices');
      NiHdr.pixdim[3] := 1;
  end;
  NiHdr.pixdim[4] := BrHdr.ACQ_repetition_time.v[1]/1000; //convert from msec to sec
  if (NiHdr.dim[3] < 2) and (BrHdr.ACQ_NSLICES > 0) and (BrHdr.ACQ_NR <= 1) and ((BrHdr.ACQ_NI mod BrHdr.ACQ_NSLICES) = 0) then
     NiHdr.dim[3] :=  BrHdr.ACQ_NSLICES;
  if (NiHdr.dim[3] < 2) and (BrHdr.ACQ_NI > 1) then
    NiHdr.dim[3] := BrHdr.ACQ_NI;
  (*if (BrHdr.RECO_wordtype_bits = 16) then begin
         //patch obsolete Bruker bug where some 32 bit EPI files report 16 bits
         // it appears this problem was fixed >10 years ago
         showmsg('Applying patch for borked Bruker EPI scans: assuming 32 (not 16) bit data');
         NiHdr.datatype := 8;
         NiHdr.bitpix := 32;
      end;
  end; *)
  if NiHdr.dim[3] > 1 then begin
     NiHdr.dim[4] := ImgBytes div  ((NiHdr.bitpix div 8 ) * NiHdr.dim[1]*NiHdr.dim[2]*NiHdr.dim[3]) ;
     if (BrHdr.ACQ_NR > 1) and (BrHdr.ACQ_NR <> NiHdr.dim[4]) then
      showmsg(' Warning: number of volumes does not match ACQ_NR');
  end else
        NiHdr.dim[3] := ImgBytes div  ((NiHdr.bitpix div 8 ) * NiHdr.dim[1]*NiHdr.dim[2]);
  if NiHdr.dim[4] > 1 then
      NiHdr.dim[0] := 4 //4D data
  else
       NiHdr.dim[0] := 3;
  str := BrHdr.ACQ_protocol_name;
  if (length(BrHdr.VISU_SeriesTypeId) > 0) and ((length(BrHdr.VISU_SeriesTypeId)+length(str)) < 79) then
      str := str + ':' + BrHdr.VISU_SeriesTypeId;
  if (length(str) > 0) and (length(str) < 80) then
      for i := 1 to length(str) do
          NiHdr.descrip[i] := str[i];
  NiHdr.slice_code:= BrHdr.METHOD_PVM_ObjOrderScheme;
  result := true;
end;

procedure swap4i(var s : LongInt);
//swap byte order of 32-bit integer, big <-> little endian
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s:=outguy.Long;
end; //proc swap4

procedure Swap32(p: pointer; ImgBytes: integer);
//swap byte order of 32-bit integer array, big <-> little endian
type
LongIntRA = array [1..1] of LongInt;
LongIntP = ^LongIntRA;
var
    i,n : integer;
    lp : LongIntP;
begin
     lp := p;
     n := ImgBytes Div 4;
     for i := 1 to n do
         swap4i(lp^[i]);
end;

procedure Swap16(p: pointer; ImgBytes: integer);
//swap array filled with 16-bit integers, big <-> little endian
type
WordRA = array [1..1] of Word;
Wordp = ^WordRA;
var
    i,n : integer;
    wp : Wordp;
begin
     wp := p;
     n := ImgBytes Div 2;
     for i := 1 to n do
         wp^[i] := swap(wp^[i]);
end;

procedure SwapDimsZT(p: Pointer; ImgBytes: integer; lNiHdr: TNiHdr; var BrHdr: TBrukerHdr);
//swizzle 3rd and 4th dimensions
type
    ByteRA0 = array [0..0] of byte;
    Bytep0 = ^ByteRA0;
var
    post, pre, xyBytes, xyzBytes, z, t, posIn, posOut, nDim, nDimNotZ, nSlices,nSlicesIn, nSlicesPreZ: integer;
    pIn, pOut: Bytep0;
begin
     //NIfTI requires time is faster changing element
     if BrHdr.VISU_Slice_Order < 2 then exit;
     xyBytes := lNiHdr.dim[1]*lNiHdr.dim[2]*(lNiHdr.bitpix div 8);
     nSlicesIn := lNiHdr.dim[3]*lNiHdr.dim[4];
     if (lNiHdr.dim[3] < 2) or ((xyBytes*nSlicesIn) <> ImgBytes) then exit;
     nDim := 0;
     nDimNotZ := 0;
     nSlices := 1;
     while (nDim < 7) and (BrHdr.VISU_FGOrder.v[nDim+1] > 0) do begin
           inc(nDim);
           if nDim = BrHdr.VISU_Slice_Order then
               lNiHdr.dim[3] := round(BrHdr.VISU_FGOrder.v[nDim])
           else begin
              nDimNotZ := nDimNotZ + 1;
              lNiHdr.dim[3+nDimNotZ] := round(BrHdr.VISU_FGOrder.v[nDim]) ;
           end;
           nSlices := nSlices * round(BrHdr.VISU_FGOrder.v[nDim]);
           if nDim < BrHdr.VISU_Slice_Order then
              nSlicesPreZ := nSlices;
     end;
     if (nSlices <>  nSlicesIn) or (nDim < 2) then begin
        showmsg('Catastrophic error decoding header');
        exit;
     end;
     lNiHdr.dim[0] := nDim + 2;
     showmsg(format(' Swapping dimensions e.g. XYTZ -> XYZT output dim %dx%dx%dx%dx%dx%d ',[lNiHdr.dim[1], lNiHdr.dim[2], lNiHdr.dim[3],
                      lNiHdr.dim[4],lNiHdr.dim[5],lNiHdr.dim[6]]));
     xyzBytes := xyBytes * lNiHdr.dim[3];
     getmem(pIn, ImgBytes);
     Move(p^,pIn^,ImgBytes);//src, dest, bytes
     pOut := Bytep0(p);
     posIn := 0;
     for post := 1 to (nSlices div (lNiHdr.dim[3] * nSlicesPreZ)) do
       for z := 1 to lNiHdr.dim[3] do
         for pre := 1 to nSlicesPreZ do begin
           posOut := ((post-1)*xyzBytes*nSlicesPreZ) + ((pre-1)*xyzBytes) + ((z-1) * xyBytes);
           Move(pIn^[posIn],pOut^[posOut], xyBytes);//src, dest, bytes
           inc(posIn, xyBytes);
         end;
     freemem(pIn);
end;

procedure Rescale(var p: pointer; var ImgBytes: integer; var BrHdr: TBrukerHdr; var NiHdr: TNiHdr);
type
   f32 = array [0..0] of single;
   f32p0 = ^f32;
   i32 = array [0..0] of int32;
   i32p0 = ^i32;
   i16 = array [0..0] of int16;
   i16p0 = ^i16;
var
   xy, xyVox, ImgBytesOut, i, z: integer;
   f : single;
   i8in: pointer;
   i16in: i16p0;
   i32in: i32p0;
   f32out: f32p0;
   nslices: integer;
   isVaries: boolean = false;
begin
  if (ImgBytes < 1) or (length(BrHdr.VISU_Intercept) < 1) or (length(BrHdr.VISU_Scaling) < 1) or (length(BrHdr.VISU_Intercept) <> length(BrHdr.VISU_Scaling)) then exit;
  nslices := NiHdr.dim[3] * NiHdr.dim[4];
  if (nslices <> length(BrHdr.VISU_Intercept)) then exit;
  for i := 0 to (nslices-1) do begin
      if (BrHdr.VISU_Intercept[i] <> BrHdr.VISU_Intercept[0]) then isVaries := true;
      if (BrHdr.VISU_Scaling[i] <> BrHdr.VISU_Scaling[0]) then isVaries := true;
  end;
  if not isVaries then begin //all slopes and intercepts identical: apply to NIfTI header
     NiHdr.scl_inter:= BrHdr.VISU_Intercept[0];
     NiHdr.scl_slope:= BrHdr.VISU_Scaling[0] ;
     exit;
  end;
  xyVox := NiHdr.dim[1]*NiHdr.dim[2];
  ImgBytesOut := xyVox*nslices*4; //we need to save as 32-bit float
  getmem(i8in, ImgBytes);
  Move(p^,i8in^,ImgBytes);//src, dest, bytes
  i16in := i16p0(i8in);
  i32in := i32p0(i8in);
  if (ImgBytes <> ImgBytesOut  ) then begin  //NIFTI requires single scaling factor for all slices: we need to save data as 32-bit float
       freemem(p, ImgBytes);
       ImgBytes := ImgBytesOut;
       getmem(p, ImgBytes);
  end;
  f32out := f32p0(p);
  i := 0;
  for z := 0 to (nslices-1) do begin
  	//if z = 0 then showmsg(format('Slice:Scaling:Offset %d : %g : %g', [z, BrHdr.VISU_Scaling[z], BrHdr.VISU_Intercept[z]]));
    for xy := 1 to xyVox do begin
        if NiHdr.bitpix = 16 then
           f := i16in^[i]
        else
            f := i32in^[i];
        //VisuCoreDataSlope - Slope values for all frames. To get the real value y for a
		//pixel x the following calculation must be performed y = slope * x + offset.
        f := (f * BrHdr.VISU_Scaling[z]) + BrHdr.VISU_Intercept[z];
        f32out^[i] := f;
        i := i + 1;
    end;
  end;
  freemem(i8in);
  NiHdr.datatype := 16; //32-bit float
  NiHdr.bitpix := 32;
  NiHdr.scl_inter:= 0;
  NiHdr.scl_slope:= 1;
end;


procedure CpyImg (lRawFName, lNiiFName: string; var ImgBytes: integer; var BrHdr: TBrukerHdr; var lNiHdr: TNiHdr);
//copy image data from Bruker 2dseq file to NIfTI .nii file: forces native byte order
var
  lOutF,lInF: File of byte;
  p: Pointer;
begin
  getmem(p, ImgBytes);
  FileMode := 0; //Set file access to read only
  AssignFile(lInF, lRawFName);
  Reset(lInF);
  BlockRead(lInF, p^, ImgBytes);
  CloseFile(lInF);
  FileMode := 2; //Set file access to read/write
  {$IFDEF ENDIAN_BIG}
  if BrHdr.RECO_byte_order_little then begin
  {$ELSE}
  if not BrHdr.RECO_byte_order_little then begin
  {$ENDIF}
    if (BrHdr.RECO_wordtype_bits = 32) then
       Swap32(p, ImgBytes);
    if (BrHdr.RECO_wordtype_bits = 16) then
       Swap16(p, ImgBytes);
  end;  //swap image data to have native endian
  Rescale(p, ImgBytes, BrHdr, lNiHdr);
  if BrHdr.VISU_Slice_Order > 1 then //suggested by Mikaël Naveau
    SwapDimsZT(p, ImgBytes, lNiHdr, BrHdr);
  AssignFile(lOutF, lNiiFName);
  Rewrite(lOutF,1);
  BlockWrite(lOutF,lNiHdr, sizeof(TNiHdr));
  BlockWrite(lOutF, p^, ImgBytes);
  CloseFile(lOutF);
  freemem(p, ImgBytes);
end;

function AppendFName (FName, AppendStr: String): string;
begin
    result := extractfilepath(FName)+AppendStr+extractfilename(FName);
end;

procedure BrConvert (lDirStr,lOutFName: string; lFOVx10, lVerbose, OnlyConvert3D, AppendProtocolName, AppendSeriesTypeId: boolean);
//convert Bruker file to NIfTI
var
    lImgDir, lBaseDir, lRecoFName, lMethodFName, lImgFName, lAcqpFName, lAcqpDir, lVisu_parsFName: string;
    BrHdr: TBrukerHdr;
    NiHdr: TNiHdr;
    ImgBytes: integer;
    ForceSFormFromReco: boolean = true;//if false : experimental support for matrices from visu file
begin
  BrHdr := BrClear;
  lBaseDir := extractfilepath(lDirStr);
  BrFindFile(lBaseDir,lAcqpFName,'acqp',false,'','');
  lAcqpDir := extractfilepath(lAcqpFName);
  if not BrFindFile(lBaseDir,lImgFName,'2dseq',true, lAcqpDir,'') then exit;//required
  lImgDir:= extractfilepath(lImgFName);
  ImgBytes := FSize(lImgFName);
  if (ImgBytes < 1) then begin showmsg('Error: Empty 2dseq file'); exit; end;
  BrFindFile(lBaseDir,lRecoFName,'reco',false, lAcqpDir,lImgDir);
  BrFindFile(lBaseDir,lMethodFName,'method',false, lAcqpDir,lImgDir);
  BrFindFile(lBaseDir,lVisu_parsFName,'visu_pars',false, lAcqpDir,lImgDir);
  if (lVisu_parsFName = '') and (lRecoFName = '') then begin
     showmsg('Unable to find a "reco" or "visu_pars" file');
     exit;
  end;
  BrReadACQP(lAcqpFName, BrHdr);
  BrReadRECO(lRecoFName, BrHdr);
  BrReadMETHOD(lMethodFName, BrHdr);
  //BrHdr.has_d3proc_file := fileexists(ExtractFilePath(lImgFName)+ 'd3proc');
  //Label files based on EXPNO and PROCNO: ACQP is in EXPNO directory 2dSeq in PROCNO directory
  BrReadVISU_PARS(lVisu_parsFName, BrHdr);
  if BrHdr.VISU_ExperimentNumber = 0 then
     BrHdr.VISU_ExperimentNumber := StrToIntDef(Extractfilename(ExtractFileDir(lAcqpFName)),123);
  if BrHdr.VISU_ProcessingNumber = 0 then
     BrHdr.VISU_ProcessingNumber := StrToIntDef(Extractfilename(ExtractFileDir(lImgFName)),321);
  if (lOutFName = '') or (DirectoryExists(lOutFName)) then begin
     if (lOutFName <> '') and (lOutFName[length(lOutFName)] <> pathdelim) then
        lOutFName := lOutFName + pathdelim; // /home/dir -> /home/dir/
     if (lOutFName = '') then
        lOutFName := ExtractFilePath(lAcqpFName);
     lOutFName := lOutFName + 'X'+inttostr(BrHdr.VISU_ExperimentNumber)+'P'+ inttostr(BrHdr.VISU_ProcessingNumber);
     if AppendProtocolName then
        lOutFName := AppendFName(lOutFName, BrHdr.ACQ_protocol_name);
    if (AppendSeriesTypeId)  then
       lOutFName := AppendFName(lOutFName, BrHdr.VISU_SeriesTypeId);
  end;
  lOutFName := changefileext(lOutFName,'.nii');
  lOutFName := UniqueFilename(lOutFName);
  if not BrHdr2NII(BrHdr, NiHdr, ImgBytes, lRecoFName) then exit;
  if (BrHdr.RECO_transposition_varies)  then begin
     showmsg('WARNING: RECO_transposition varies, slices have different orientations (can not create a NIfTI volume, probably a localizer scan): '+ lImgFName);
     if OnlyConvert3D then
        exit;
  end;
  BrReport(BrHdr, lRecoFName, lOutFName, lVerbose);
  if (not BrHdr.VISU_visu_exists) or ((BrHdr.RECO_reco_exists) and (ForceSFormFromReco)) then
      BrMat (BrHdr, NiHdr, lFOVx10, (lVerbose) and (not BrHdr.VISU_visu_exists))
  else
      BrMat_VISU (BrHdr, NiHdr, lFOVx10, lVerbose) ;
  CpyImg (lImgFName, lOutFName, ImgBytes, BrHdr, NiHdr);
  setlength(BrHdr.VISU_Scaling,0); //free memory
  setlength(BrHdr.VISU_Intercept,0); //free memory
end;

procedure Find2Dseq(Dir, OutFName: string; FOVx10, lVerbose, OnlyConvert3D, AppendProtocolName, AppendSeriesTypeID: boolean; var nOK: integer);
//Find and convert any "2dseq" file in Dir and enclosed sub-directories
var
  SR: TSearchRec;
begin
  if FindFirst(IncludeTrailingBackslash(Dir) + '*', faAnyFile or faDirectory, SR) = 0 then
    try
      repeat
        if ((SR.Attr and faDirectory) = 0) and (SR.Name = '2dseq') then begin
          BrConvert(IncludeTrailingBackslash(Dir)+SR.Name,OutFName, FOVx10, lVerbose, OnlyConvert3D, AppendProtocolName, AppendSeriesTypeID);
          inc(nOK);
        end else if (SR.Name <> '.') and (SR.Name <> '..') then
          Find2Dseq(IncludeTrailingBackslash(Dir) + SR.Name, OutFName, FOVx10, lVerbose, OnlyConvert3D, AppendProtocolName, AppendSeriesTypeID, nOK);  // recursive call!
      until FindNext(Sr) <> 0;
    finally
      FindClose(SR);
    end;
end;

procedure BrConvertBatch (FName, OutFName: string; FOVx10, lVerbose, OnlyConvert3D, AppendProtocolName, AppendSeriesTypeID: boolean);
var
  nOK: integer = 0;
begin
  showmsg(kVers);
  if not DirectoryExists(FName) then
    FName := ExtractFilePath(FName);
  if OutFName = '' then begin
    if not DirectoryExists(FName) then
      OutFName := ExtractFilePath(FName)
    else
        OutFName := FName;
  end;
  Find2Dseq(Fname, OutFName, FOVx10, lVerbose, OnlyConvert3D, AppendProtocolName, AppendSeriesTypeID, nOK);
  if nOK = 0 then
    showmsg('Unable to find any "2dseq" files in '+FName)
  else
      showmsg('Found '+inttostr(nOK)+' 2dseq in '+FName);
end;
