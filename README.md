##### Introduction

This is a simple tool for converting Bruker ParaVision MRI data to the NIfTI file format. It includes both a drag-and-drop graphical interface (Bru2Nii) as well as a command line tool (Bru2). The compiled tools run on Windows, Linux and OSX without requiring any other files to be installed. The source code can be built using Lazarus without any other tools required. This project is inspired by the Perl script pvconv (http://pvconv.sourceforge.net).

You can find pre-compiled binary executables in the 'compiled' folder, or if you wish you can re-compile your own copy. 

##### Usage (Command Line)

Provide the name of the Bruker format 'acqp' or 'subject' file you wish to convert. Here are a few examples for Windows
 * Convert all data from a subject `Bru2 c:\mydata\subject`
 * Convert single session `Bru2 c:\mydata\10\acqp`
 * Convert, appending protocol name to output filename `Bru2 -p c:\mydata\10\acqp`
 * Convert to specific folder `Bru2 -o c:\output c:\mydata\10\acqp`
Here are examples for Unix
 * Convert all data from a subject `Bru2 /Users/cr/dir/subject`
 * Convert single session `Bru2 /Users/cr/dir/acqp`
 * Convert, appending protocol name to output filename `Bru2 -p /Users/cr/dir/acqp`
 * Convert to specific folder `Bru2 -o /Users/cr/dir2/out /Users/cr/dir/acqp`
    
##### Usage (Graphical Interface)

Drag and drop the Bruker 'acqp' or 'subject' file you wish to convert. 

![alt tag](https://github.com/neurolabusc/Bru2Nii/blob/master/gui.png)

##### Compile from Source

1. You will want to install Lazarus (http://www.lazarus-ide.org). For Windows, download and run the unified installer. For Linux and OSX, you will want to install the lazarus, fpc and fpc-src packages.
2. You can build these tools from the graphical interface. For example, launch Lazarus. Select Project/OpenProject and then choose project you wish to compile (for example Bru2Nii.lpr). Finally, choose the Run/Run command. 
3. To compile the console version from the command line, run either one of these commands "fpc Bru2.lpr" or "lazbuild -B Bru2.lpr".
4. To build the graphical version from the command line, run the command "lazbuild -B Bru2Nii.lpr". Note by default OSX will compile to 32-bit Carbon. If you have a recent SVN of Lazarus (50307 or later) you can compile as 64-bit Cocoa with "lazbuild ./Bru2Nii.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/bin/ppcx64""
5. You may optionally strip you executables to make them require less disk space, e.g. "strip ./Bru2Nii", "strip ./Bru2" - the compiler will probably do this automatically for Windows or Linux, but not OSX (due to the debugger).

##### Versions

2June2015 : 
 - Original Beta version
 - Need examples to support images with multiple echoes (e.g. separate volumes from T2+PD acquisition
 - Need to get examples for setting origin and rotation correctly
 
11Nov2015 : 
 - Handles slice gap seen in some files (issue identified by Horea Christian)
 - Bru2 -o option no longer adds search path (issue identified by Horea Christian)
 
##### License

Being inspired by a Perl script we maintain the same license (http://dev.perl.org/licenses/) as the original pvconv project.

##### Links

 * [pvconv](http://pvconv.sourceforge.net) converts Bruker data to the older Analyze format (and therefore does not retain spatial orientation information). [github page](https://github.com/matthew-brett/pvconv)
 * [Bruker2Analyze](http://www.mccauslandcenter.sc.edu/mricro/mricro/bru2anz/) is another Bruker to Analyze conversion tool. 
 * Matthew Brett notes that the example Bruker data text files report themselves as being in [JCAMP-DX
+format](http://jcamp-dx.org/). In particular, the example files say they are in JCAMP-DX [4.24 format](http://jcamp-dx.org/protocols/dxir01.pdf).
 * [dsi-studio](http://dsi-studio.labsolver.org/Manual/Parse-DICOM) can extract diffusion directions from Bruker datasets.
 * [mriutil](http://www.pennstatehershey.org/web/nmrlab/resources/software/mriutil) can view Bruker images without needing to convert them.