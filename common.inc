const
  kVers = 'Bruker2NIfTI 11 November 2015 by Matthew Brett, Andrew Janke, Chris Rorden. See www.mricro.com';
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
  TMat = array[1..4, 1..4] of single;
  TBrukerHdr = Record
    RECO_byte_order_little, RECO_transposition_varies: boolean;
    RECO_transposition, RECO_wordtype_bits, ACQ_ns_list_size, ACQ_nr_completed, ACQ_NR, ACQ_NI, ACQ_NSLICES: integer;
    ACQ_slice_thick,ACQ_read_offset, ACQ_phase1_offset, METHOD_slice_offset, METHOD_slice_distance: single;
    RECO_offset, RECO_size, RECO_fov, ACQ_grad_matrix, ACQ_slice_sepn, ACQ_repetition_time : TVec;
    ACQ_protocol_name : string[79];
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

procedure SaveNiHdr (lHdrName: string; lNiHdr: TNiHdr);
//save 348 byte NIfTI header
var
lF: file;
lHdr: TNiHdr;
begin
  lHdr := lNiHdr;
  FileMode := 2; //read/write
  AssignFile(lF,lHdrName);
  Rewrite(lF,sizeof(TNiHdr));
  BlockWrite(lF,lHdr, 1);
  CloseFile(lF);
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

function BrFindFile (var lxBaseDir,lxPath: string; lxFileName: string; lPrompt: boolean): boolean;
//start from basedir and search sub-folders for a file named filename.
var
   lxSearchDir,lxNewDir : string;
   lxSearchRec: TSearchRec;
begin
     result := false;
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
     result.METHOD_slice_distance := 0;
     result.RECO_size:= BrVecZero;
     result.RECO_offset := BrVecZero;
     result.RECO_fov := BrVecZero;
     result.ACQ_slice_sepn := BrVecZero;
     result.ACQ_repetition_time := BrVecZero;
     result.ACQ_grad_matrix := BrVecZero;
end;

function BrArrayVaries(var lFile: TextFile; lFname: string; var lVaries: boolean): single;
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

function BrGetArray(var lFile: TextFile;  lStr: string; var lRA: TVec): single;
//read array from Bruker header
var
   lStart,lEnd, n: integer;
begin
     result := 0;
     if Eof(lFile) then exit;
     lStart := AnsiPos('(', lStr)+1;
     lEnd := AnsiPos(')', lStr);
     //s := trim(Copy(lStr,lStart, lEnd-lStart));
     n := strtoint(trim(Copy(lStr,lStart, lEnd-lStart)));
     if n < 1 then
      exit
     else if n = 1 then
        ReadLn(lFile, lRA.v[1])
     else if n = 2 then
          ReadLn(lFile, lRA.v[1], lRA.v[2])
     else if n = 3 then
          ReadLn(lFile, lRA.v[1], lRA.v[2], lRA.v[3])
     else if n < 9 then
          ReadLn(lFile, lRA.v[1], lRA.v[2], lRA.v[3], lRA.v[4])
     else begin   //ACQ_grad_matrix
          ReadLn(lFile, lRA.v[1], lRA.v[2], lRA.v[3], lRA.v[4], lRA.v[5], lRA.v[6], lRA.v[7], lRA.v[8], lRA.v[9]);
     end;
     result := lRA.v[1];
end;

function BrGetInt (var lStr: string): integer;
//read integer from Bruker header
begin
     result := StrToInt(RightStr(lStr, length(lStr)-AnsiPos('=', lStr)));
end;

function BrGetFloat (var lStr: string): single;
//read float from Bruker header
begin
     result := StrToFloat(RightStr(lStr, length(lStr)-AnsiPos('=', lStr)));
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

function BrReadMETHOD (fname: string; var BrHdr: TBrukerHdr): boolean;
//read Bruker format Method file
var
   lFile: TextFile;
   lStr :string;
   lUnused: TVec;
begin
  result := false;
  if not fileexists(fname) then exit;
  assignfile(lFile,fname);
  reset(lFile);
  while not Eof(lFile) do begin
        ReadLn(lFile, lStr);
        if AnsiPos('##$PVM_SPackArrSliceOffset', lStr) = 1 then
           BrHdr.METHOD_slice_offset := BrGetArray(lFile, '(1)',lUnused );
        if AnsiPos('##$PVM_SPackArrSliceDistance', lStr) = 1 then
           BrHdr.METHOD_slice_distance := BrGetArray(lFile, '(1)',lUnused );
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
  if not fileexists(fname) then exit;
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
        if AnsiPos('##$ACQ_NSLICES=', lStr) = 1 then
           BrHdr.ACQ_NSLICES := BrGetInt(lStr);
        if AnsiPos('##$NR=', lStr) = 1 then
           BrHdr.ACQ_NR := BrGetInt(lStr);
        if AnsiPos('##$ACQ_nr_completed', lStr) = 1 then
           BrHdr.ACQ_nr_completed := BrGetInt(lStr);
  end;
  result := true;
  closefile(lFile);
end;

function BrReadRECO (fname: string; var BrHdr: TBrukerHdr): boolean;
//Read Bruker format RECO file
var
    lFile: TextFile;
    lStr :string;
begin
  result := false;
  if not fileexists(fname) then exit;
  assignfile(lFile,fname);
  reset(lFile);
  BrHdr.RECO_wordtype_bits := 32; //default
  BrHdr.RECO_byte_order_little:= false; //default
  while not Eof(lFile) do begin
        ReadLn(lFile, lStr);
        if AnsiPos('##$', lStr) <> 1 then continue;
        if AnsiPos('##$RECO_wordtype=_16BIT_SGN_INT', lStr) = 1 then
           BrHdr.RECO_wordtype_bits := 16;
        if AnsiPos('##$RECO_transposition=', lStr) = 1 then
           BrHdr.RECO_transposition  := round(BrArrayVaries(lFile, fname, BrHdr.RECO_transposition_varies ));
        if AnsiPos('##$RECO_byte_order=littleEndian', lStr) = 1 then
           BrHdr.RECO_byte_order_little:= true;
        if AnsiPos('##$RECO_size=', lStr) = 1 then
           BrGetArray(lFile, lStr, BrHdr.RECO_size);
        if AnsiPos('RECO_offset=', lStr) = 1 then
           BrGetArray(lFile, '(3)', BrHdr.RECO_offset);
        if AnsiPos('##$RECO_fov=', lStr) = 1 then
           BrGetArray(lFile, lStr, BrHdr.RECO_fov);
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
     if BrHdr.RECO_byte_order_little then
        showmsg(' Endian: Little')
     else
          showmsg(' Endian: Big');
     showmsg(' Bits : '+ inttostr(BrHdr.RECO_wordtype_bits));
     showmsg(' Size : '+f2str(BrHdr.RECO_size.v[1])+'x'+f2str(BrHdr.RECO_size.v[2])) ;
     showmsg(' FOV : '+f2str(BrHdr.RECO_fov.v[1])+'x'+f2str(BrHdr.RECO_fov.v[2]));
     showmsg(' Matrix = ['+f2str(BrHdr.ACQ_grad_matrix.v[1])+' '+f2str(BrHdr.ACQ_grad_matrix.v[2])+' '+f2str(BrHdr.ACQ_grad_matrix.v[3])+'; '+
      f2str(BrHdr.ACQ_grad_matrix.v[4])+' '+f2str(BrHdr.ACQ_grad_matrix.v[5])+' '+f2str(BrHdr.ACQ_grad_matrix.v[6])+'; '+
      f2str(BrHdr.ACQ_grad_matrix.v[7])+' '+f2str(BrHdr.ACQ_grad_matrix.v[8])+' '+f2str(BrHdr.ACQ_grad_matrix.v[9])+']');
     showmsg(' Sepn (Thick+Gap) : '+f2str(BrHdr.ACQ_slice_sepn.v[1]));
     showmsg(' Slice Thick : '+f2str(BrHdr.ACQ_slice_thick));
     showmsg(' Offset (Read/Phase/Slice) : '+f2str(BrHdr.ACQ_read_offset)+' '+f2str(BrHdr.ACQ_phase1_offset)+' '+f2str(BrHdr.METHOD_slice_offset));
     showmsg(' METHOD_slice_distance : '+   f2str(BrHdr.METHOD_slice_distance));
     showmsg(' RECO_offset : '+f2str(BrHdr.RECO_offset.v[1])+' '+f2str(BrHdr.RECO_offset.v[2])+' '+f2str(BrHdr.RECO_offset.v[3]));
     showmsg(' Repetition Time : '+f2str(BrHdr.ACQ_repetition_time.v[1]) );
     showmsg(' NI (images per TR, slices): '+inttostr(BrHdr.ACQ_NI));
     showmsg(' NSLICES : '+inttostr(BrHdr.ACQ_NSLICES));
     showmsg(' NR (volumes): '+inttostr(BrHdr.ACQ_NR));
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
  if lVerbose then begin //display transformation values
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
  NiHdr.sform_code:= kNIFTI_XFORM_SCANNER_ANAT;
  NiHdr.srow_x[0] := so_far[1,1]; NiHdr.srow_x[1] := so_far[1,2]; NiHdr.srow_x[2] := so_far[1,3]; NiHdr.srow_x[3] := so_far[1,4];
  NiHdr.srow_y[0] := so_far[2,1]; NiHdr.srow_y[1] := so_far[2,2]; NiHdr.srow_y[2] := so_far[2,3]; NiHdr.srow_y[3] := so_far[2,4];
  NiHdr.srow_z[0] := so_far[3,1]; NiHdr.srow_z[1] := so_far[3,2]; NiHdr.srow_z[2] := so_far[3,3]; NiHdr.srow_z[3] := so_far[3,4];
end;

function BrHdr2NII (BrHdr: TBrukerHdr; var NiHdr: TNiHdr; ImgBytes: integer; var FName: string): boolean;
//convert Bruker Header format to NIfTI format
var
    i: integer;
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
  for i := 1 to 4 do
      NiHdr.dim[i] := round(BrHdr.RECO_size.v[i]);
  if (NiHdr.dim[1] < 1) or (NiHdr.dim[2] < 1) then begin
     showmsg('Error: first 2 dimensions of RECO_size must be non-zero'+FName);
     exit;
  end;
  if (NiHdr.dim[3] < 1) then NiHdr.dim[3]  := 1;
  for i := 1 to 2 do begin
        if BrHdr.RECO_size.v[i] = 0 then begin
          NiHdr.pixdim[i] := 1;
          showmsg('Warning: FOV does not describe all dimensions (localizer scan?) '+FName);
        end else
            NiHdr.pixdim[i] := (10*BrHdr.RECO_fov.v[i])/BrHdr.RECO_size.v[i] ;   //x10 FOV is CM not MM
  end;
  if BrHdr.METHOD_slice_distance > 0 then
    NiHdr.pixdim[3] := BrHdr.METHOD_slice_distance //best source for distance between slices : includes gap
  else if BrHdr.RECO_size.v[3]> 0 then
    NiHdr.pixdim[3] := (10*BrHdr.RECO_fov.v[3])/BrHdr.RECO_size.v[3] //distance between slices, x10 FOV is CM not MM
  else
    NiHdr.pixdim[3] := BrHdr.ACQ_slice_thick; //BrHdr.ACQ_slice_sepn.v[1]; //distance between slices
  NiHdr.pixdim[4] := BrHdr.ACQ_repetition_time.v[1]/1000; //convert from msec to sec
  if (NiHdr.dim[3] < 2) and (BrHdr.ACQ_NI > 1) then
    NiHdr.dim[3] := BrHdr.ACQ_NI;
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
  if (length(BrHdr.ACQ_protocol_name) > 0) and (length(BrHdr.ACQ_protocol_name) < 80) then
      for i := 1 to length(BrHdr.ACQ_protocol_name) do
          NiHdr.descrip[i] := BrHdr.ACQ_protocol_name[i];
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

procedure CpyImg (lRawFName, lNiiFName: string; ImgBytes: integer; BrHdr: TBrukerHdr);
//copy image data from Bruker 2dseq file to NIfTI .nii file: forces native byte order
var
  lOutF,lInF: File of byte;
  p: Pointer;
begin
   getmem(p, ImgBytes);
  AssignFile(lInF, lRawFName);
  Reset(lInF);
  FileMode := 0; //Set file access to read only
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
  AssignFile(lOutF, lNiiFName);
  Reset(lOutF);
  Seek(lOutF, sizeof(TNiHdr));
  BlockWrite(lOutF, p^, ImgBytes);
  CloseFile(lOutF);
  freemem(p, ImgBytes);
end;

function AppendFName (FName, AppendStr: String): string;
begin
    result := extractfilepath(FName)+AppendStr+extractfilename(FName);
end;

procedure BrConvert (lDirStr,lOutFName: string; lFOVx10, lVerbose, OnlyConvert3D, AppendProtocolName: boolean);
//convert Bruker file to NIfTI
var
    lBaseDir, lRecoFName, lMethodFName, lImgFName, lAcqpFName: string;
    BrHdr: TBrukerHdr;
    NiHdr: TNiHdr;
    ImgBytes: integer;
begin
  lBaseDir := extractfilepath(lDirStr);
  if not BrFindFile(lBaseDir,lRecoFName,'reco',true) then exit;
  if not BrFindFile(lBaseDir,lAcqpFName,'acqp',true) then exit;
  if not BrFindFile(lBaseDir,lImgFName,'2dseq',true) then exit;
  showmsg('');
  BrFindFile(lBaseDir,lMethodFName,'method',true); //optional - do not exit if missing
  ImgBytes := FSize(lImgFName);
  BrHdr := BrClear;
  if not BrReadACQP(lAcqpFName, BrHdr) then exit;
  if not BrReadRECO(lRecoFName, BrHdr) then exit;
  BrReadMETHOD(lMethodFName, BrHdr);
  if lOutFName = '' then
    lOutFName := ExtractFilePath(lRecoFName)+Extractfilename(ExtractFileDir(lRecoFName))+'.nii';
   if AppendProtocolName then
      lOutFName := AppendFName(lOutFName, BrHdr.ACQ_protocol_name);
  lOutFName := UniqueFilename(lOutFName);
  if not BrHdr2NII(BrHdr, NiHdr, ImgBytes, lRecoFName) then exit;
  if (ImgBytes < 1) then begin showmsg('Error: Empty 2dseq file'); exit; end;
  if (BrHdr.RECO_transposition_varies)  then begin
     showmsg('WARNING: slices have different orientations (can not create a NIfTI volume, probably a localizer scan): '+ lImgFName);
     if OnlyConvert3D then
        exit;
  end;
  BrReport(BrHdr, lRecoFName, lOutFName, lVerbose);
  BrMat (BrHdr, NiHdr, lFOVx10, lVerbose);
  SaveNiHdr (lOutFName, NiHdr);
  CpyImg (lImgFName, lOutFName, ImgBytes, BrHdr);
end;

procedure BrConvertBatch (FName, OutFName: string; FOVx10, lVerbose, OnlyConvert3D, AppendProtocolName: boolean);
//convert a Bruker format file
var
   lOK: integer;
   lSearchRec: TSearchRec;
   lParentDirACQP, lSearchDir,lFileName,lOutFilename, lS: String;
begin
  if DirectoryExists(FName) then
     lSearchDir := FName
  else
      lSearchDir := extractfiledir(FName) ;
  lOK := 0;
  //1st: look for acqp in current folder - user specified acqp or method file
  lFileName := lSearchDir+pathdelim+'acqp';
  if fileexists(lFilename) then begin
   if length(OutFName) < 1 then
      lOutFilename := lSearchDir+pathdelim +ExtractFileName(lSearchDir)+'.nii'
   else
      lOutFilename :=  OutFName+'.nii';
   inc(lOK);
   BrConvert (lFileName,lOutFilename, FOVx10, lVerbose, OnlyConvert3D, AppendProtocolName);
   lS := extractfilename (FName);
   if (AnsiPos('acqp', lS) = 1) then
        exit; //quit: batch convert 'subject' files, not single images 'acqp'
  end;
  //2nd: look for 2dseq in current folder - e.g. user specified 2dseq or reco file
  lFileName := lSearchDir+pathdelim+'2dseq';
  if fileexists(lFilename) then begin
      lParentDirACQP := extractfilepath(extractfiledir(lSearchDir))+'acqp';
      if fileexists(lParentDirACQP) then begin
         lSearchDir := extractfiledir(lParentDirACQP) ;
         if length(OutFName) < 1 then
            lOutFilename := lSearchDir+pathdelim +ExtractFileName(lSearchDir)+'.nii'
         else
            lOutFilename :=  OutFName+'.nii';
         inc(lOK);
         BrConvert (lFileName,lOutFilename, FOVx10, lVerbose, OnlyConvert3D, AppendProtocolName);
      end;
  end;
  //3rd last resort: assume we are in subject folder and look for acqp in sub-directors
  if FindFirst(lSearchDir+pathdelim+'*', faDirectory, lSearchRec) = 0 then begin
   repeat  //open each folder in succession
           if (lSearchRec.Name <> '.') and (lSearchRec.Name <> '..') and (( lSearchRec.Attr and faDirectory) <> 0) then begin
            lFileName := lSearchDir+pathdelim +lSearchRec.Name+pathdelim+'acqp';
              if fileexists(lFilename) then begin
                 if length(OutFName) < 1 then
                  lOutFilename := lSearchDir+pathdelim +lSearchRec.Name+'.nii'
                 else
                     lOutFilename :=  OutFName+'.nii';
               inc(lOK);
               BrConvert (lFileName,lOutFilename, FOVx10, lVerbose, OnlyConvert3D, AppendProtocolName);
              end;
           end;
     until (FindNext(lSearchRec) <> 0);
  end;
  FindClose(lSearchRec); //free resources
  if lOK = 0 then
   showmsg('Unable to find any Bruker acqp files, e.g. '+ lFileName);
end;
