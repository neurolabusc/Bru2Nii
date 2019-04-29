##### Introduction

**This software is no longer actively maintained. Please use Bruker's own [ParaVision 360](https://github.com/neurolabusc/Bru2Nii/issues/24)**

This is a simple tool for converting Bruker ParaVision MRI data to the NIfTI file format. While every attempt has been made to provide robust support, the ParaVision format is inherently complicated, contradictory, poorly documented, and has included explicit errors. One is left wondering if this format is intentionally obfuscated. New versions have defined features and added new features that appear to conflict with existing older features. Therefore, it is often unclear what the correct solution is, and different tools (see links below) may provide different solutions. Users should use any Bruker conversion tools with extreme caution and encourage the vendor to directly support simpler formats (e.g. creating a NIfTI "hdr" header file when the Paravision files are generated could be done seamlessly and would greatly aid customers). As a professional company, Bruker has an obligation and incentive to help users access their data. Variations of this project have existed since 1999, and yet the situation has not improved. Any customer considering purchasing equipment or services from this company should demand they develop a cleaner, more transparent solution that will reduce costs in terms of support and potentially erroneous results.

This project includes both a drag-and-drop graphical interface (Bru2Nii) as well as a command line tool (Bru2). The compiled tools run on Windows, Linux and OSX without requiring any other files to be installed. The source code can be built using Lazarus without any other tools required. This project is inspired by the Perl script [pvconv](http://pvconv.sourceforge.net).

You can find pre-compiled binary executables with the latest [release](https://github.com/neurolabusc/Bru2Nii/releases). Alternatively, one can recompile the latest version.

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

##### Installation

The easiest way to use this software is to [download an executable compiled for your operating system](https://github.com/neurolabusc/Bru2Nii/releases). Versions are provided for Windows, MacOS and Linux. There are two programs: a command line version and a graphical interface.

##### Compile from Source

1. You will want to install [Lazarus](http://www.lazarus-ide.org). For Windows, download and run the unified installer. For Linux and OSX, you will want to install the lazarus, fpc and fpc-src packages.
2. You can build these tools from the graphical interface. For example, launch Lazarus. Select Project/OpenProject and then choose project you wish to compile (for example Bru2Nii.lpr). Finally, choose the Run/Run command.
3. To compile the console version from the command line, run either one of these commands "fpc Bru2.lpr" or "lazbuild -B Bru2.lpr".
4. To build the graphical version from the command line, run the command "lazbuild -B Bru2Nii.lpr". Note by default OSX will compile to 32-bit Carbon. If you have a recent SVN of Lazarus (50307 or later) you can compile as 64-bit Cocoa with "lazbuild ./Bru2Nii.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/bin/ppcx64""
5. You may optionally strip you executables to make them require less disk space, e.g. "strip ./Bru2Nii", "strip ./Bru2" - the compiler will probably do this automatically for Windows or Linux, but not OSX (due to the debugger).

##### Versions

3March2018 : v 1.0.20180303
 - [Option to create compressed (.nii.gz) files](https://github.com/neurolabusc/Bru2Nii/issues/19).

##### License

Being inspired by a Perl script we maintain the same license (http://dev.perl.org/licenses/) as the original pvconv project.

##### Links
 * Bruker [ParaVision 360](https://github.com/neurolabusc/Bru2Nii/issues/24) supports NIfTI export.
 * [mrtrix3 convert_bruker](https://github.com/MRtrix3/mrtrix3/blob/5b5ef203090fe8c615a7f35708422a50cc89eb00/bin/convert_bruker).
 * [bruker2nifti_qa](https://gitlab.com/naveau/bruker2nifti_qa/tree/master) provides sample Bruker datasets to validate conversion.
 * [bruker2nifti](https://github.com/SebastianoF/bruker2nifti) is a scriptable Python tool for conversion.
 * [Bruker2nifti](https://github.com/CristinaChavarrias/Bruker2nifti) is a scriptable Matlab tool for conversion.
 * [pvconv](http://pvconv.sourceforge.net) converts Bruker data to the older Analyze format (and therefore does not retain spatial orientation information). [github page](https://github.com/matthew-brett/pvconv).
 * [Bruker2Analyze](http://www.mccauslandcenter.sc.edu/mricro/mricro/bru2anz/) is another Bruker to Analyze conversion tool.
  * [dicomifier can convert Bruker to DICOM and then DICOM to NIfTI](https://github.com/lamyj/dicomifier).
 * Matthew Brett notes that the example Bruker data text files report themselves as being in [JCAMP-DX
+format](http://jcamp-dx.org/). In particular, the example files say they are in JCAMP-DX [4.24 format](http://jcamp-dx.org/protocols/dxir01.pdf).
 * [dsi-studio](http://dsi-studio.labsolver.org/Manual/Parse-DICOM) can extract diffusion directions from Bruker datasets.
 * [nanconvert uses ITK's convert Bruker 2dseq to other formats supported by ITK](https://github.com/spinicist/nanconvert) (including NIfTI).

 * [mriutil](http://www.pennstatehershey.org/web/nmrlab/resources/software/mriutil) can view Bruker images without needing to convert them.