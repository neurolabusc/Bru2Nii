unit mat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

Type
  mat44 = array [0..3, 0..3] of Single;
  vect4 = array [0..3] of Single;
  mat33 = array [0..2, 0..2] of Single;
  vect3 = array [0..2] of Single;
  ivect3 = array [0..2] of integer;
  ByteRA = array [1..1] of byte;
  function Mat2Str(name: string; m: mat44): string; overload;
  function Mat2Str(name: string; m: mat33): string; overload;
  function Vec2Str(name: string; v: vect3): string;
  function Bool2Str(name: string; b: boolean): string;
  function toMatrix (r11,r12,r13,r21,r22,r23,r31,r32,r33: double): mat33;
  function Vector3 (a,b,c: double): vect3;
  function compute_affine_from_visu_pars(vc_orientation: mat33; vc_position: vect3; vc_subject_position: string; resolution: vect3;
                                    frame_body_as_frame_head : boolean =False; keep_same_det : boolean=True;
                                    consider_subject_position : boolean =False): mat44;
  procedure nifti_mat44_to_quatern( lR :mat44;
                             var qb, qc, qd,
                             qx, qy, qz,
                             dx, dy, dz, qfac : single);

implementation

function Bool2Str(name: string; b: boolean): string;
begin
   if b then
      result := name + ' = true'
   else
       result := name + ' = false';
end;

function Vec2Str(name: string; v: vect3): string;
begin
   result := format('%s = [%g %g %g]',[name,v[0],v[1],v[2] ]);
end;

function Mat2Str(name: string; m: mat33): string; overload;
begin
   result := format('%s = [%g %g %g; %g %g %g; %g %g %g]',[name,
          m[0,0],m[0,1],m[0,2],
          m[1,0],m[1,1],m[1,2],
          m[2,0],m[2,1],m[2,2]
          ]);
end;

function Mat2Str(name: string; m: mat44): string; overload;
begin
   result := format('%s = [%g %g %g %g; %g %g %g %g; %g %g %g %g; %g %g %g %g]',[name,
          m[0,0],m[0,1],m[0,2],m[0,3],
          m[1,0],m[1,1],m[1,2],m[1,3],
          m[2,0],m[2,1],m[2,2],m[2,3],
          m[3,0],m[3,1],m[3,2],m[3,3]
          ]);
end;

function Vector3 (a,b,c: double): vect3;
begin
   result[0] := a;
   result[1] := b;
   result[2] := c;
end;

procedure fromMatrix (m: mat44; var r11,r12,r13,r21,r22,r23,r31,r32,r33: double);
begin
  r11 := m[0,0];
  r12 := m[0,1];
  r13 := m[0,2];
  r21 := m[1,0];
  r22 := m[1,1];
  r23 := m[1,2];
  r31 := m[2,0];
  r32 := m[2,1];
  r33 := m[2,2];
end;

function toMatrix (r11,r12,r13,r21,r22,r23,r31,r32,r33: double): mat33;
begin
  result[0,0] := r11;
  result[0,1] := r12;
  result[0,2] := r13;
  result[1,0] := r21;
  result[1,1] := r22;
  result[1,2] := r23;
  result[2,0] := r31;
  result[2,1] := r32;
  result[2,2] := r33;
end;

function diag(v: vect3): mat33;
begin
  result := toMatrix(v[0],0,0, 0,v[1],0, 0,0,v[2]);
end;

function nifti_mat44_determ(a: mat44): double;
//Translated by Chris Rorden, from C function "nifti_mat44_inverse"
// Authors: Bob Cox, revised by Mark Jenkinson and Rick Reynolds
// License: public domain
// http://niftilib.sourceforge.net
//Note : For higher performance we could assume the matrix is orthonormal and simply Transpose
//Note : We could also compute Gauss-Jordan here
var
	r11,r12,r13,r21,r22,r23,r31,r32,r33: double;//,v1,v2,v3 , deti : double;
begin
   r11 := a[0,0]; r12 := a[0,1]; r13 := a[0,2];  //* [ r11 r12 r13 v1 ] */
   r21 := a[1,0]; r22 := a[1,1]; r23 := a[1,2];  //* [ r21 r22 r23 v2 ] */
   r31 := a[2,0]; r32 := a[2,1]; r33 := a[2,2];  //* [ r31 r32 r33 v3 ] */
   //v1  := a[0,3]; v2  := a[1,3]; v3  := a[2,3];  //* [  0   0   0   1 ] */
   result := r11*r22*r33-r11*r32*r23-r21*r12*r33
		 +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;
   //if( deti <> 0.0 ) then
   //	deti := 1.0 / deti ;
end;

function nifti_mat33_determ( R: mat33 ):double;   //* determinant of 3x3 matrix */
begin
  result := r[0,0]*r[1,1]*r[2,2]
           -r[0,0]*r[2,1]*r[1,2]
           -r[1,0]*r[0,1]*r[2,2]
           +r[1,0]*r[2,1]*r[0,2]
           +r[2,0]*r[0,1]*r[1,2]
           -r[2,0]*r[1,1]*r[0,2] ;
end;

function nifti_mat33_rownorm( A: mat33 ): single;  // max row norm of 3x3 matrix
var
   r1,r2,r3: single ;
begin
   r1 := abs(A[0,0])+abs(A[0,1])+abs(A[0,2]);
   r2 := abs(A[1,0])+abs(A[1,1])+abs(A[1,2]);
   r3 := abs(A[2,0])+abs(A[2,1])+abs(A[2,2]);
   if( r1 < r2 ) then r1 := r2 ;
   if( r1 < r3 ) then r1 := r3 ;
   result := r1 ;
end;

procedure fromMatrix33 (m: mat33; var r11,r12,r13,r21,r22,r23,r31,r32,r33: double);
begin
  r11 := m[0,0];
  r12 := m[0,1];
  r13 := m[0,2];
  r21 := m[1,0];
  r22 := m[1,1];
  r23 := m[1,2];
  r31 := m[2,0];
  r32 := m[2,1];
  r33 := m[2,2];
end;


function nifti_mat44_inverse(a: mat44): mat44;
//Translated by Chris Rorden, from C function "nifti_mat44_inverse"
// Authors: Bob Cox, revised by Mark Jenkinson and Rick Reynolds
// License: public domain
// http://niftilib.sourceforge.net
//Note : For higher performance we could assume the matrix is orthonormal and simply Transpose
//Note : We could also compute Gauss-Jordan here
var
	r11,r12,r13,r21,r22,r23,r31,r32,r33,v1,v2,v3 , deti : double;
begin
   r11 := a[0,0]; r12 := a[0,1]; r13 := a[0,2];  //* [ r11 r12 r13 v1 ] */
   r21 := a[1,0]; r22 := a[1,1]; r23 := a[1,2];  //* [ r21 r22 r23 v2 ] */
   r31 := a[2,0]; r32 := a[2,1]; r33 := a[2,2];  //* [ r31 r32 r33 v3 ] */
   v1  := a[0,3]; v2  := a[1,3]; v3  := a[2,3];  //* [  0   0   0   1 ] */
   deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
		 +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;
   if( deti <> 0.0 ) then
	deti := 1.0 / deti ;
   result[0,0] := deti*( r22*r33-r32*r23) ;
   result[0,1] := deti*(-r12*r33+r32*r13) ;
   result[0,2] := deti*( r12*r23-r22*r13) ;
   result[0,3] := deti*(-r12*r23*v3+r12*v2*r33+r22*r13*v3
                      -r22*v1*r33-r32*r13*v2+r32*v1*r23) ;
   result[1,0] := deti*(-r21*r33+r31*r23) ;
   result[1,1] := deti*( r11*r33-r31*r13) ;
   result[1,2] := deti*(-r11*r23+r21*r13) ;
   result[1,3] := deti*( r11*r23*v3-r11*v2*r33-r21*r13*v3
                      +r21*v1*r33+r31*r13*v2-r31*v1*r23) ;
   result[2,0] := deti*( r21*r32-r31*r22) ;
   result[2,1] := deti*(-r11*r32+r31*r12) ;
   result[2,2] := deti*( r11*r22-r21*r12) ;
   result[2,3] := deti*(-r11*r22*v3+r11*r32*v2+r21*r12*v3
                      -r21*r32*v1-r31*r12*v2+r31*r22*v1) ;
   result[3,0] := 0; result[3,1] := 0; result[3,2] := 0.0 ;
   if (deti = 0.0) then
        result[3,3] := 0
   else
       result[3,3] := 1;//  failure flag if deti == 0
end;

function nifti_mat33_inverse( R: mat33 ): mat33;   //* inverse of 3x3 matrix */
var
   r11,r12,r13,r21,r22,r23,r31,r32,r33 , deti: double ;
begin
   FromMatrix33(R,r11,r12,r13,r21,r22,r23,r31,r32,r33);
   deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
         +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;
   if( deti <> 0.0 ) then deti := 1.0 / deti ;
   result[0,0] := deti*( r22*r33-r32*r23) ;
   result[0,1] := deti*(-r12*r33+r32*r13) ;
   result[0,2] := deti*( r12*r23-r22*r13) ;
   result[1,0] := deti*(-r21*r33+r31*r23) ;
   result[1,1] := deti*( r11*r33-r31*r13) ;
   result[1,2] := deti*(-r11*r23+r21*r13) ;
   result[2,0] := deti*( r21*r32-r31*r22) ;
   result[2,1] := deti*(-r11*r32+r31*r12) ;
   result[2,2] := deti*( r11*r22-r21*r12) ;
end;

function nifti_mat33_colnorm( A: mat33 ): single;  //* max column norm of 3x3 matrix */
var
   r1,r2,r3: single ;
begin
   r1 := abs(A[0,0])+abs(A[1,0])+abs(A[2,0]) ;
   r2 := abs(A[0,1])+abs(A[1,1])+abs(A[2,1]) ;
   r3 := abs(A[0,2])+abs(A[1,2])+abs(A[2,2]) ;
   if( r1 < r2 ) then r1 := r2 ;
   if( r1 < r3 ) then r1 := r3 ;
   result := r1 ;
end;

function nifti_mat33_polar( A: mat33 ): mat33;
var
   k:integer;
   X , Y , Z: mat33 ;
   dif,alp,bet,gam,gmi : single;
begin
  dif := 1;
  k := 0;
   X := A ;
   gam := nifti_mat33_determ(X) ;
   while( gam = 0.0 )do begin        //perturb matrix
     gam := 0.00001 * ( 0.001 + nifti_mat33_rownorm(X) ) ;
     X[0,0] := X[0,0]+gam ;
     X[1,1] := X[1,1]+gam ;
     X[2,2] := X[2,2] +gam ;
     gam := nifti_mat33_determ(X) ;
   end;
   while true do begin
     Y := nifti_mat33_inverse(X) ;
     if( dif > 0.3 )then begin     // far from convergence
       alp := sqrt( nifti_mat33_rownorm(X) * nifti_mat33_colnorm(X) ) ;
       bet := sqrt( nifti_mat33_rownorm(Y) * nifti_mat33_colnorm(Y) ) ;
       gam := sqrt( bet / alp ) ;
       gmi := 1.0 / gam ;
     end else begin
       gam := 1.0;
       gmi := 1.0 ;  //close to convergence
     end;
     Z[0,0] := 0.5 * ( gam*X[0,0] + gmi*Y[0,0] ) ;
     Z[0,1] := 0.5 * ( gam*X[0,1] + gmi*Y[1,0] ) ;
     Z[0,2] := 0.5 * ( gam*X[0,2] + gmi*Y[2,0] ) ;
     Z[1,0] := 0.5 * ( gam*X[1,0] + gmi*Y[0,1] ) ;
     Z[1,1] := 0.5 * ( gam*X[1,1] + gmi*Y[1,1] ) ;
     Z[1,2] := 0.5 * ( gam*X[1,2] + gmi*Y[2,1] ) ;
     Z[2,0] := 0.5 * ( gam*X[2,0] + gmi*Y[0,2] ) ;
     Z[2,1] := 0.5 * ( gam*X[2,1] + gmi*Y[1,2] ) ;
     Z[2,2] := 0.5 * ( gam*X[2,2] + gmi*Y[2,2] ) ;
     dif := abs(Z[0,0]-X[0,0])+abs(Z[0,1]-X[0,1])+abs(Z[0,2]-X[0,2])
           +abs(Z[1,0]-X[1,0])+abs(Z[1,1]-X[1,1])+abs(Z[1,2]-X[1,2])
           +abs(Z[2,0]-X[2,0])+abs(Z[2,1]-X[2,1])+abs(Z[2,2]-X[2,2]);
     k := k+1 ;
     if( k > 100) or (dif < 3.e-6 ) then begin
         result := Z;
         break ; //convergence or exhaustion
     end;
     X := Z ;
   end;
   result := Z ;
end;

procedure nifti_mat44_to_quatern( lR :mat44;
                             var qb, qc, qd,
                             qx, qy, qz,
                             dx, dy, dz, qfac : single);
var
   r11,r12,r13 , r21,r22,r23 , r31,r32,r33, xd,yd,zd , a,b,c,d : double;
   P,Q: mat33;  //3x3
begin
   // offset outputs are read write out of input matrix
   qx := lR[0,3];
   qy := lR[1,3];
   qz := lR[2,3];
   //load 3x3 matrix into local variables
   fromMatrix(lR,r11,r12,r13,r21,r22,r23,r31,r32,r33);
   //compute lengths of each column; these determine grid spacings
   xd := sqrt( r11*r11 + r21*r21 + r31*r31 ) ;
   yd := sqrt( r12*r12 + r22*r22 + r32*r32 ) ;
   zd := sqrt( r13*r13 + r23*r23 + r33*r33 ) ;
   //if a column length is zero, patch the trouble
   if( xd = 0.0 )then begin r11 := 1.0 ; r21 := 0; r31 := 0.0 ; xd := 1.0 ; end;
   if( yd = 0.0 )then begin r22 := 1.0 ; r12 := 0; r32 := 0.0 ; yd := 1.0 ; end;
   if( zd = 0.0 )then begin r33 := 1.0 ; r13 := 0; r23 := 0.0 ; zd := 1.0 ; end;
   //assign the output lengths
   dx := xd;
   dy := yd;
   dz := zd;
   //normalize the columns
   r11 := r11/xd ; r21 := r21/xd ; r31 := r31/xd ;
   r12 := r12/yd ; r22 := r22/yd ; r32 := r32/yd ;
   r13 := r13/zd ; r23 := r23/zd ; r33 := r33/zd ;
   { At this point, the matrix has normal columns, but we have to allow
      for the fact that the hideous user may not have given us a matrix
      with orthogonal columns. So, now find the orthogonal matrix closest
      to the current matrix.
      One reason for using the polar decomposition to get this
      orthogonal matrix, rather than just directly orthogonalizing
      the columns, is so that inputting the inverse matrix to R
      will result in the inverse orthogonal matrix at this point.
      If we just orthogonalized the columns, this wouldn't necessarily hold.}
   Q :=  toMatrix (r11,r12,r13,          // 2D "graphics" matrix
                           r21,r22,r23,
                           r31,r32,r33);
   P := nifti_mat33_polar(Q) ; //P is orthog matrix closest to Q
   FromMatrix33(P,r11,r12,r13,r21,r22,r23,r31,r32,r33);
{                           [ r11 r12 r13 ]
 at this point, the matrix  [ r21 r22 r23 ] is orthogonal
                            [ r31 r32 r33 ]
 compute the determinant to determine if it is proper}

   zd := r11*r22*r33-r11*r32*r23-r21*r12*r33
       +r21*r32*r13+r31*r12*r23-r31*r22*r13 ; //should be -1 or 1

   if( zd > 0 )then begin // proper
     qfac  := 1.0 ;
   end else begin //improper ==> flip 3rd column
     qfac := -1.0 ;
     r13 := -r13 ; r23 := -r23 ; r33 := -r33 ;
   end;
   // now, compute quaternion parameters
   a := r11 + r22 + r33 + 1.0;
   if( a > 0.5 ) then begin  //simplest case
     a := 0.5 * sqrt(a) ;
     b := 0.25 * (r32-r23) / a ;
     c := 0.25 * (r13-r31) / a ;
     d := 0.25 * (r21-r12) / a ;
   end else begin  //trickier case
     xd := 1.0 + r11 - (r22+r33) ;// 4*b*b
     yd := 1.0 + r22 - (r11+r33) ;// 4*c*c
     zd := 1.0 + r33 - (r11+r22) ;// 4*d*d
     if( xd > 1.0 ) then begin
       b := 0.5 * sqrt(xd) ;
       c := 0.25* (r12+r21) / b ;
       d := 0.25* (r13+r31) / b ;
       a := 0.25* (r32-r23) / b ;
     end else if( yd > 1.0 ) then begin
       c := 0.5 * sqrt(yd) ;
       b := 0.25* (r12+r21) / c ;
       d := 0.25* (r23+r32) / c ;
       a := 0.25* (r13-r31) / c ;
     end else begin
       d := 0.5 * sqrt(zd) ;
       b := 0.25* (r13+r31) / d ;
       c := 0.25* (r23+r32) / d ;
       a := 0.25* (r21-r12) / d ;
     end;
     if( a < 0.0 )then begin b:=-b ; c:=-c ; d:=-d; {a:=-a; this is not used} end;
   end;
   qb := b ;
   qc := c ;
   qd := d ;
end;

function round_mat44(a: mat44; Digits: integer ): mat44;
var
   i,j: integer;
begin
   for i := 0 to 3 do
       for j := 0 to 3 do
           result[i,j] := roundto(a[i,j],Digits);//roundto(a[i,j], Digits);
end;

function eye(): mat44;
begin
     result[0,0]:=1;result[0,1]:=0;result[0,2]:=0;result[0,3]:=0;
     result[1,0]:=0;result[1,1]:=1;result[1,2]:=0;result[1,3]:=0;
     result[2,0]:=0;result[2,1]:=0;result[2,2]:=1;result[2,3]:=0;
     result[3,0]:=0;result[3,1]:=0;result[3,2]:=0;result[3,3]:=1;
end;

function mat44tomat33(a: mat44): mat33;
var
   i,j: integer;
begin
   for i := 0 to 2 do
       for j := 0 to 2 do
           result[i,j] := a[i,j];
end;

procedure sanity_check_visu_core_subject_position(vc_subject_position: string);
begin
     if (vc_subject_position <> 'Head_Prone') and (vc_subject_position <> 'Head_Supine') then
        raise Exception.CreateFmt('Known cases are "Head_Prone" or  "Head_Supine" for the parameter "visu_pars.VisuSubjectPosition" not "%s"', [vc_subject_position]);
end;

function matrixMult(a, b: mat33): mat33; overload;
var i,j: integer;
begin
   for i := 0 to 2 do begin
       for j := 0 to 2 do begin
           result[i, j] := A[i, 0] * B[0,j]
           + A[i, 1] * B[1, j]
           + A[i, 2] * B[2, j];
       end;  //for j
   end; //for i
end; //multiplymatrices()

function pivot(a: mat33; col: integer): double;
begin
     result := a[0,col];
     if (abs(a[1,col]) > abs(result)) then
        result := a[1,col];
     if (abs(a[2,col]) > abs(result)) then
        result := a[2,col];
end;

function compute_affine_from_visu_pars(vc_orientation: mat33; vc_position: vect3; vc_subject_position: string; resolution: vect3;
                                  frame_body_as_frame_head : boolean =False; keep_same_det : boolean=True;
                                  consider_subject_position : boolean =False): mat44;
(*Pascal port of https://github.com/SebastianoF/bruker2nifti/blob/master/bruker2nifti/_utils.py
Copyright (c) 2017 Sebastiano Ferraris

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
    How the affine is computed:
    0) resolution, orientation and translation are provided in separate array, we combine them togheter in the
    standard 4x4 matrix.
    1) We invert the resulting matrix - according to conventions ParaVision (scanner to image frame)
       and DICOM/Nifti (image to scanner frame).
    2) impose the signs of the first two columns (pivots) to be negative, and the third to be be positive.
    - according to the fact that the provided transformation is DICOM-like (LPS) instead of NIFTI like (RAS)
    (Left/Right, Anterior/Posterior, Inferior/Superior).
    -------- optional changes ----------
    3) frame_body_as_frame_head: Switching the last 2 columns of the rotational part, no matter the value of
    VisuCoreTransposition - According to the fact we are dealing with quadrupeds and not with humans,
    we need to switch the Anterior-Posterior with the Inferior-Superior direction.
    Set frame_body_as_frame_head=True to set the biped orientation.
    4) consider_subject_position: This can be 'head_prone' or 'head_supine'.
    Reason why sometimes this must be considered for a correct
    orientation and must be considered dis-jointly with frame_body_as_frame_head, is that this parameter is sometimes
    tuned to voluntarily switch from radiological to neurological coordinate systems.
    If the subject is Prone and the technician wants to have the coordinates in neurological he/she can consciously
    set the variable vc_subject_position to 'Head_Supine', even if the subject is not supine.
    5) keep_same_det: Finally, for safety, we can impose the same determinant as the input matrix.
    (If there is any b-vectors list, this should be modified accordingly).
    :param vc_orientation: visu core orientation parameter.
    :param vc_position: visu core position parameter. -  corresponds to the translational part of the matrix.
    :param vc_subject_position: 'Head_Prone' or 'Head_Supine'. If head supine and if consider_subject_position is True
    it invert the direction of the axis anterior-posterior. - do not confuse subject_position with positon (read this
    last as 'translation').
    :param resolution: resolution of the image, output of compute_resolution_from_visu_pars in the same module.
    :param frame_body_as_frame_head: [False] This standard is the standard for me and my dataset. To get the
    behaviour described in the manual set to False.
    :param keep_same_det: in case you want the determinant to be the same as the input one. Consider it in particular
    if frame_body_as_frame_head is set to False, and according to the choice of consider_subject_position.
    :param consider_subject_position: [False] The reason why sometimes this must be considered for a correct
    orientation and sometimes must not, is that this parameter is tuned to voluntarily switch from radiological
    to neurological coordinate systems. If the subject is Prone and the technician wants to have the coordinates
    in neurological he/she can consciously set the variable vc_subject_position to 'Head_Supine'.
    :return: final affine (qform) transformation according to the nifti convention
    NOTE: we are assuming that the angles parametrisation is the same for the input and the output.
    We hope this is the case as we do not have any mean to confirm that. The fslreorient2std from FSL
    should be applied afterwards to all the images (after DWI analysis if any).
*)
var
   i,j: integer;
   result_det: double;
   result_orientation: mat33;
begin


    sanity_check_visu_core_subject_position(vc_subject_position);
    //vc_orientation = filter_orientation(vc_orientation); //ignore

    //# 0) integrate resolution with the orientation and add the translation in the projective coordinates:

    result := eye;//result = np.eye(4, dtype=np.float32)
    for i := 0 to 2 do
        for j := 0 to 2 do
            result[i,j] := vc_orientation[i,j]; //result[0:3, 0:3] = vc_orientation
    for i := 0 to 2 do
        result[i,3] := vc_position[i];  //result[0:3, 3] = vc_position


    //# 1) Invert the orientation matrix, according to nifti convention and Bruker manual.
    //# Round the decimals to avoid precision problems. Check if determinant makes sense.

    //result := round_mat44(result, 4);
    result := round_mat44(nifti_mat44_inverse(result), -4); //result = np.round(np.linalg.inv(result), decimals=4)

    result_det := nifti_mat44_determ(result);   //result_det = np.linalg.det(result)
    if result_det = 0 then
       raise Exception.Create('Orientation determinant is 0. Cannot grasp this dataset');
    //# 2-3) impose pivot first column negative, second column negative, third column positive

    result_orientation := mat44tomat33(result);

    if not frame_body_as_frame_head then
       result_orientation := matrixMult(result_orientation, toMatrix(1,0,0, 0,0,1, 0,1,0));
       //https://docs.scipy.org/doc/numpy/reference/generated/numpy.dot.html
       // For 2-D arrays it is equivalent to matrix multiplication
       // result_orientation := result_orientation.dot(np.array([[1, 0, 0], [0, 0, 1], [0, 1, 0]]))
    //showmessage(mat2str('r',result_orientation));
    if pivot(result_orientation, 0) > 0 then
       for i := 0 to 2 do
           result_orientation[i, 0] := -1 * result_orientation[i, 0];
    if pivot(result_orientation, 1) > 0 then
       for i := 0 to 2 do
           result_orientation[i, 1] := -1 * result_orientation[i, 1];
    if pivot(result_orientation, 2) < 0 then
       for i := 0 to 2 do
           result_orientation[i, 2] := -1 * result_orientation[i, 2];
    result_orientation := matrixMult(result_orientation, diag(resolution));
    for i := 0 to 2 do
        for j := 0 to 2 do
            result[i,j] := result_orientation[i,j];
    //# 4) - optional

    if consider_subject_position then
        if vc_subject_position = 'Head_Prone' then
           for i := 0 to 3 do
            result[1, i] := -1 * result[1, i];
    //# 5) - optional
    if keep_same_det then
        if ((nifti_mat44_determ(result) < 0) and (0 < result_det)) or ((nifti_mat44_determ(result) > 0) and (0 > result_det)) then
           for i := 0 to 2 do
               result[0, i] := -1 * result[0, i];
end;

end.

