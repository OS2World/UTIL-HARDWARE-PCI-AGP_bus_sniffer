README for PCI32.ZIP

This is a very quick note to explain PCI32.

PCI32 is just a port of my 16-bit DOS program 'PCI' to "true" win32 - that is, functionality under the 32-bit x86 builds of Windows NT 4.0, 2000, XP, 2003, Longhorn/Vista  and so on. This program will NOT work under DOS/WIN9x/ME/OS2/Linux etc, or any 64-bit OS!!!

PCI32 is tested and works under any Microsoft win32 OS e.g. NT4.0, Win2000, XP, 2003, & "Longhorn/Vista", including all server versions and other variants (media centre, SBS server, tablet edition, enterprise edition, etc). I have never tested it under a cluster, but I would expect it to report on the attributes of whatever hardware node it's actually run on.

The new reader is recommended to read PCI.DOC (Included in the archive) for historical information regarding this software.

It is reported that PCI32 does work under a 32-bit WinPE Environment (such as BartPE), although I do not personally test it for use in that environment, and continued functionality is not guaranteed. I will try not to break WinPE compatibility, as WinPE is the sort of area that PCI32 proves to be of great use - I quite simply don't have the resources to guarantee it.

Full documentation on the more abstract aspects of PCI32 such as the PCIDEVS.TXT official file format can be found in its sister program, PCI. Some functions not relevant in the win32 environment have been removed - pci32 /? lists the functions that work...

Support website:	http://members.datafast.net.au/dft0802
Email:			chart (at) datafast (dot) net (dot) au			(Damn Spam!!)


How to use it?

PCI32 is a console mode program - you run it from the command prompt. The results appear in text form in the command prompt window, just like "good old" MS-DOS... There is no 'GUI' interface.

General advice for beginners:

ú	Unzip pci32 to a new folder e.g. C:\PCI32. Use winzip or winrar or windows XP's built-in "extract all" command. You should get a bunch of files, including this document.
ú	Open the command prompt (start - run - (type in) CMD or start - all programs - accessories - command prompt) and you get a black screen with white text.
ú	Type in C:
ú	Type in cd \pci32
ú	Type in pci32
ú	Type in exit to close the black screen, when you're finished

Your report will probably scroll off the top of the screen. Use the window's vertical scrollbar slider to go back "up" and read from the top, or use the send-to-notepad method as per below:

Like all good console mode software, it takes command line parameters (try pci32 /?), and can have it's output 'piped' to a file or device. So, if you really hate console mode, do this:

PCI32 > report.txt
notepad report.txt

Which will generate the report and then launch notepad with the results opened. You can then cut/paste/print/email/etc to your hearts content. As a free bonus, the file 'report.txt' is saved for you to come back to in future. Wow :-)

Also, check out the available options. PCI32 /? Will tell you all about lots of other useful functions!


If it won't run...

- You need at least PCI32.EXE, GWIOPM.SYS and PCIDEVS.TXT present for it to run. If any file is missing, it won't work.

- You need administrator privileges to run (or at least, rights to install and start device drivers). This is because PCI32 seamlessly installs a device driver (gwiopm.sys) in order to directly accesses the PCI hardware. Gwiopm.sys is removed when the program exits - nothing left behind in memory or the registry and no need to reboot. How cool is that? This also means you can run it on a 'live' server (from a floppy, memory stick, CD, whatever). Handy, that.

- You need to run it from a local disk drive; it will not run from a mapped drive or UNC path, because windows will not allow device drivers which are not located on local drives, to be loaded.

- You must run it from the command prompt, NOT by double-clicking on it from explorer or via start-run. Otherwise the results just scroll by and the window closes.

- PCI32's driver seems to conflict with "motherboard monitor" by LiveWireDev.com Remove MBM (and reboot) before using PCI32. MBM is now outdated, unreliable, unsupported software and should be removed anyhow.

- PCI32 is a 32-bit program and will NOT function under any 64-bit version of Windows eg XP-64, server 2003 64-bit edition, IA64 or A64 or Itanium 64-bit OS'S, nor will it run on plain DOS or 16-bit windows 95/98/ME/3.x or OS/2 (use plain PCI for these systems).

- Always remember that help is available by running pci32 /? You'll also discover some interesting extra features there.

- If none of the above makes sense, bear in mind that this is a technical tool written for technicians. Not to be nasty, but if the above is too confusing, perhaps you probably don't need whatever PCI32 does.



Updating the PCI database

The value of any PCI program is reflected directly by how current it's PCI device database (pcidevs.txt) is. If the database is too old, modern hardware won't be recognised, and therefore the program's net worth is much reduced. To counter this issue, I actively maintain and update the database on a daily basis. New (and previously unknown) devices are regularly added, and existing entries are updated to more accurately reflect the actual hardware or fix recognition issues. Updated lists are published (on average) about twice a month, or as often as updates are received and processed.

All list entries are hand-edited, no scripts or automated procedures are permitted - this means that "garbage" is kept out of the list. Most other lists found on the web are full of errors, but this list is not, thanks to the validation of having a human actually checking the data as it is entered. 

Of course, typos and just plain incorrect information do make it to the list from time to time; therefore if you ever find an error, please email me the details, and it will be corrected immediately.


Your Contributions Are Requested

All this is only possible, however, if I receive update contributions from YOU, the public. I cannot possibly gain access to every piece of PCI hardware ever built, nor can I spend all my time trolling about people's computers, manufacturer manuals, driver .INF's and websites trying to locate PCI ID's. Here's how you can help:

If you have any information on a PCI device which is not in my database, including the Windows driver's .INF file, a list of device ID's, web links, specifications documents, dumps from my PCI programs, etc etc. please send them to me!! My email address is listed at the top of this document.

Contributions from hardware developers are especially appreciated. If you would like your products to be instantly recognisable, by your official product name, to a large number of diagnostic tools as well as the Linux and FreeBSD operating systems, then you should send me your ID's! Listing is of course completely free of charge, and you can be sure that your products will be correctly recognised using your preferred wording and official product naming conventions.

To add an entry to the database, at minimum I need the PCI Vendor ID, Device ID, and a device description. Any bonuses would be information pertaining to device revisions, subsystem ID's, previous company names for the vendor, details of product families, details on how to tell similar products apart, and so on.

The easiest way to do this is to get the .INF file from the Windows driver for the hardware, and send that to me. A Windows driver .INF file contains everything I need to know, and every driver must have an .INF file (Although it may sometimes be hidden inside an installer archive). As a bonus, .INF's often list a number of devices from the same product family, so I may be able to add recognition of a whole group of products, just from one INF!

 

A commentary on PCI databases

My database 'PCIDEVS.TXT' is the most extensive listing I can find on the web. There are other listings, such as the Linux list (pci.ids) and BSD list, however they don't keep track of as much basic information such as chip revisions, nor do they have as much "raw data" as mine. If you think my list is out of date, don't complain about it, contact me and help me make the list better for everyone! Be aware, however, that lists exist with large numbers of errors or are simply out of date. Ask yourself when and how the list is maintained before concluding that another list is better/bigger/more accurate.

Many other PCI databases are actually just my list, reformatted or merged with other lists to form a new list. My list has a few subtle characteristics, which indicate list re-packaging, such as the vendor names with a (was: xxx) edit, and a few deliberate typos here and there. Whilst I fully support the use of my data in other programs, the wholesale cut-and-paste of my list by simply renaming, reformatting or deliberate alteration of the comments to hide it's true origin disgusts me, and I urge you to report any such lists to me so that I may take action against the parties involved. It is my understanding at this time that both the Linux and FreeBSD lists are the [partial] result of merges of my list. It's absolutely OK by me for them to do this, as they asked my permission first, and I granted it. Anyone else merging my list with other lists is doing so without my permission or approval. Remember, the list is Copyright, and although you are free to use it AS-IS, you cannot alter it or call it yours. (See the legal section at the end).

In summary: You can use this list for anything you want, but only if you keep it intact, as-is - don't pollute it with merges from other lists or remove any part of the list, especially the comments at the top and bottom. If you think this list needs updating, don't merge your stuff in and keep it to yourself; please send ME the merge-data so that this list gets better!

I strongly urge developers to use the list in your programs, rather than start your own list: my list is absolutely free, easy to use, regularly updated, and contains far more info than you could possibly assemble yourself if you started from scratch today. All I ask is that you mention this website in your software, so your users know where to go to get updates, and that you keep pcidevs.txt intact (no editing please), and as a separate file - don't compile it into your program, don't rename the file, and don't try to "hide" it's true source by editing it to remove my name and links.

You are welcome to come back to my website and download for yourself the most current database as often as you like. It's completely free, even for commercial use. There are no catches - this is a hobby of mine, not a money making enterprise.



Compiling the program

100% of all source and object code required to recompile yourself is supplied. You don't need anything else besides a copy of Delphi to recompile yourself. I compile with Borland Delphi 7.0 standard (Delphi 4 works; other versions of Delphi are untested and may or may not work). Do *NOT* load into Delphi's GUI!!! At the command prompt run dcc32 pci32.pas to compile (Ignore the warnings with gwiopm.pas)!

Several other people's code is used in PCI32 (Device Driver & NT driver subsystem stuff) - read all the source code files to see who did what.



Legal mumbo Jumbo

PCI32 is freeware. Use it, it's free. Also, no need to ask before you incorporate this code into commercial software. Credit would be appreciated if you use parts of the code in your own programs, and an email would be nice if you do find a good use for it - I'll be happy to mention your software on my website. As with all freeware products, the code is provided as-is, and no warranty or guarantee of fitness for any purpose is implied.

PCIDEVS.TXT is also freeware, HOWEVER it is also Copyright. You are NOT permitted to alter or edit the file in any way, particularly not to remove my name from it or add yours. This is my list, you may USE and distribute it as-is, but you may not alter it. Consider the file READ-ONLY! You may make derivative works based on my file; eg you may merge the data contained within my list with yours to improve your list. Acknowledgement of my contribution, which is visible to your end-user, is required.

GWIOPM.SYS is not mine. Before using that driver software, please read it's licence and decide for yourself if you can use it. The home of GWIOPM.SYS is at http://www.wideman-one.com/gw/tech/Delphi/iopm/index.htm

Publishers wishing to include PCI32 on a free 'Cover CD' and/or write a review article on PCI32 for publication, are free to do so without making additional requests for approval; however the article must include attribution to the author "Craig Hart" and reference the official website http://members.datafast.net.au/dft0802

Other than that, please use it at your leisure.



 How to "parse" PCIDEVS.TXT

Writing your own code to use my database? GREAT! You're more than welcome: the database is freeware and you may use it for any purpose, including commercial purposes. Here's how *I* parse the list, in "pseudocode". I hope this makes it easier for you to write a parser too. Please forgive me if you think this is poorly designed/could be done better; the technique stems from three things:

- Sourcefile is TEXT, not Binary, so there are no fixed field lengths.
- Many fields are optional so the catch-22 of "you dunno if it's there 'till you read it, but once you've read it, you can't go back a line if it wasn't what you expected" applies.
- The program has been added to and expanded "ad hoc" over a 5 year period. Inevitable oversights and extensions have been dealt with several times.
- The basic file format can't be changed since there are many programs (including a couple of commercial packages) relying on my file format not changing.





Open the file as text (not binary) and read top to bottom, one line at a time.

1. Look for V entries, until you make a match to the vendor ID, or EOF in which case report unknown vendor & device. Display Vendor Name.

<the file position pointer now points to the list of devices for that Vendor>


2. Read D entries until you get a match to the device ID, or hit another V entry or EOF in which case stop and report unknown device. DONT display Device Name yet!!

<the file position pointer now points to possible device revision records>


3. Read R entries until you get a match with the device's revision ID, or you run out of R entries, or EOF. (There may be no R entries at all). If you match R, then display that entry for the Device Name, otherwise display the Device Name from the D entry... R is more accurate than D, but you might not match R.

<the file position pointer now points to possible subsystem records>


4. If you matched D(and/or R), and the subsystem ID is <>00000000 then (look
for subsystem match):

   in a loop, read a line, and
     - try to match an X code with the 8 digit subsystem ID.
     - try to match an O code with your subsystem vendor ID. Remember the
       "generic" part description. Note the match, Keep going in this loop.
     - try to match an S code with your subsystem device ID, but only if the O
       code has already been matched.
   exit when you matched an X code, or (matched an S AND an O code), EOF, or
   you hit another V or D code.

- if you EOF or matched neither X, O nor S, report unknown subsystem ID.
- if you matched an X entry, report info next to X entry, and warn user this is a known "oddball" device that has an otherwise invalid subsystem ID. (Ignore any partial S-but-not-O match, if any - its a false alarm).
- if you matched O BUT NOT S, report the "generic" ID you remembered. Warn user it may not be "fully" accurate, but is just a guestimation.
- if you matched O AND S, report info next to S entry; you exactly matched the subsystem ID.

5. close list

6. If you have an O code, re-open the list. Scan list reading the V entries, but try to match with the O code. This tells you the OEM name. Stop at match or EOF. If EOF, report unknown OEM ID. Close list.

List must be closed and re-opened because the O name may be "higher" up the list, thus a scan-to-the-end of the list will not match. This also means this check must be done last since it causes us to "loose our spot" in the list. Filepos() doesn't work (in Pascal, at least) since we're working with a textfile!! (Argh!!)

If you find an invalid code letter, IGNORE IT; just move onto the next line.
This lets us add new code letters and not 'break' existing code. Also, ignore any invalid or null lines, or lines starting with a ";" character.




 ç Other formatting notes for PCIDEVS.TXT:

All useful lines are formatted thus:

<Code Letter><Tab><2-8 hex digits><tab><text>

You may NOT replace <tab> with <space>! Also, do NOT replace a tab with eight spaces!!! the parser counts characters left to right, and looks for the tab character, so wrong, extra or missing characters will result in a wrongly parsed line. This means the file formatting must be kept strictly in check at all times. Use my CHKPCI utility (Available seperately from the website) to inspect and validate your changes.

Overall, total line length must be 255 characters or less (Pascal language limitation). Try to keep the text under about 70 chars for display legibility (excessive display wordwrap really is in poor taste :-/)

All entries must always follow numerical order, lowest to highest. This makes duplicates almost impossible when editing, but the parser doesn't actually care, since it works on "keep looking until you run out" principle. A "tiny" efficiency could be added by coding in "if database ID > our ID, give up" but I hardly see the point, since the program runs faster than you can blink anyhow.

You should ignore all lines not starting with a valid code letter,tab sequence. This allows clarity by inserting blank lines and comments wherever it may please you. For clarity, begin comment lines with a <;> character; Accidental capitilisation of the first word of a comment could lead to a wrongly parsed code.

Valid Codes:

  V	Vendor ID. 4 digit hex number.
  D	Device ID. 4 digit hex number.
  R	Revision ID. 2 digit hex number.
  X	Incorrectly formatted susbsystem ID. 8 digit hex number.
  O	Subsystem OEM ID. 4 digit hex number. (top 16 bits of subsystem ID)
  S	Subsystem device ID. 4 digit hex number. (low 16 bits of subsystem ID)

The codes must always appear in this order in the file. Multiple O and S
entries may appear. O entries may appear without S entries. Only V and D
entries are required to identify a device - all others are optional and my be
omitted.

A note on R entries: R is NOT permitted under a subsystem entry. A chipset revision is just that - the BASE CHIPSET's revision. The OEM can't have any influence on the chipset's revision, since he doesn't make it! Thus, R is of no use under the subsystem. I very much doubt any OEM has made two different model cards by carefully buying different revision chips from the chipset vendor!!

A note on X entries: X entries are very rare. In the "early days" of subsystem ID's, some vendors apparently thought they had carte blanche to put in any number they liked. WRONG! However a few devices now exist with nonsensical subsystem ID's like "55555555" or "F0F01234" and suchlike. X entries take care of these few "oddball" devices. I don't expect to add any more than one or two new X entries, ever.

** New code letters may be added from time to time, so your code should always ignore any unknown code letters. This lets up expand the scope of the file without 'breaking' existing code.






Thanks, and so on

It would be impossible to individually thank everyone who has contributed something towards PCI/PCI32 over the years, however the following people have gone out of their way to help over and extended period, and so an extra special thankyou goes out to them now.

Ralph Brown, Ray Hinchliffe, Konstantin Koll, Sergei Shtylyov, Alex Kossenkov, Bill Avery III, C. Adrian Silasi, Gunther Mayer, Stefan Danes, and finally, Veit Kannegieser.
	
Thanks guys, without your outstanding efforts, both the PCI code and the device database would not be anywhere near as good as they are today.

A very quick final mention goes out to Veit Kannegieser, who maintains a much enhanced, OS/2-specific build of PCI tailored to the needs of OS/2 users, and who has by far contributed the most in the way of bug fixes, enhancements, ideas and so forth. You will find a link to Veit's version on the website Thanks Veit!



Program Revision Info


Version 0.50á
- inital relase. Crude, rushed port from DOS Version. Seems to work OK, but:
	no color
	no page-pause
	some diagnostic modes removed as not relevant to win32 environment
	driver requires administrative privileges to load
	filesize 81k!!


Version 0.51á
- added decode of driver load/unload error messages
- added self-repair of previous failed driver-install to fix an issue where the driver stuffs up if it couldn't load for a reason, and then PCI32 was re-run. (Usually caused by no gwiopm.sys or not run with admin privileges)
- Added APIC mode detection & comment to IRQ info.
- Added run from anywhere feature - pci32 can now run from a path and still load pcidevs.txt. Unsure if UNC path works yet, but is likely...
- got colour working in console display output.

Version 0.52á
- Added major features to power management capability decoding
- fixed major bugs with:
    printstatus() routine always dropping first message
    expROM sizing (again!)
- removed code left over from DOS port which is not actually called in program!
- will now run from a UNC path correctly in some cases; needs more testing to be certain...
- updated with AGP 8x (Version 3.0) support
- updated with 'classic' PCI v2.3 & v3.0 specs:
     New capabilities:
	HyperTransport Capability
	AGP 8x capability
	Secure Device capability
	PCI Express capability
	MSI-X capability
- updated with basic PCI Express support; much more to come yet
    PCI Express support is experimental and untested on actual hardware so far...

Version 0.53á
- Bunch of bugfixes to do with scanning multi-function devices. This bug has existed since the very first, original version of PCI!! 
- Obscure bug with revision checking in subsystem code causing a GPF fixed
  (Bug only visible on Intel PCIe chipsets which are unknown to pcidevs.txt)

Version 1.0
- Decided to drop the 'beta' label once and for all; hence version 1.0
- More issues with Expansion ROM detection/sizing fixed
- Fixed problems with hex dump corruption (caused by ExpROM decode test)
- some fixes to PCI Express code

Version 1.1
- Fixed a big bug with configuration space write code - which was not actually writing anything sane into the PCI Configuration space registers. This has finally fixed the elusive Expansion Rom code once and for all - I Promise!
- Added decoding of Power Management data register, if present.
- Added maximum bus latency and minimum bus grant timer info to reports
- Added -R option to draw a "tree" of the hardware Bus:Device:Function structure
- Finally fixed log standing, stupid code bug with subsystems vs R entries.
- Minor fix-ups to PCI Express code; more to follow yet.
- Depreciated priority of legacy 'PCI' 16-bit version, which will continue to be maintained, but is given less priority than PCI32 in future. Expect to see new features in PCI32 first, with back-porting, where possible, later.

Version 1.2
- Fixed bug with an un-initialised variable in PCI
- Totally overhauled bus type (-B option) detection. Much better at determining bus type (PCI, AGP, PCIe or CardBus) correctly.
- Added PCI-X bus type guessing to -B option
- A few other minor bugfixes
- Added -z debugging option: PCI32 shows a bit more debugging info to help developers with troubleshooting code faults (will not be useful in normal use; developers only!!).

Version 1.3
- Major improvements to the -R option, as follows:
-R now draws all the tree lines correctly in all cases; previously it would draw the lines incorrectly if the last device on a bus was a multifunction type device.
- R now shows any detected busses with no present devices; this is useful to report the existence of a known, but unpopulated bus e.g. a CardBus port or PCI-hotplug bus with no inserted card(s).
-R now shows the class code info of each device. This makes it easier to visualise the bus layout without having to refer to the main part of the report.
-R now lists the bridge bus numbers to make visualising bus to bus bridge layout easier.

- Altered code to allow for any number of PCI buses (up to the maximum of 255 allowed by the PCI standard), by following the chain of bridges until no more bridges exist. This removes the need for the workaround that formerly dealt with BIOS-unconfigured CardBus busses, and cleans up the code nicely as a result.
- Altered installer mode I: parameter: Now written in HEX rather than decimal, because under windows values up to 255 are legal, and it was screwing up the indentation. 
- Another slight fix for -I mode: trailing space on each line removed
Anyone using the I: parameter of installer mode will probably need to re-write their code to account for these changes.
- Removed driver related messages from top and bottom of all output. This cleans up installer mode's reports so that they come out the same way PCI does them.
- Added PCI-X 2.0 support to PCI-X capability decoding; bugfixes to PCI-X support in general
- PCI32 now detects when it's I/O is redirected, and uses regular ASCII characters where appropriate, to enable clear viewing in Windows with fixed-width fonts (lucidia, courier new, terminal, etc).
- PCI32 now recognises the OS type and reports it in the Searching info at the top of the report.
- more code cleanups for readability and conversion to modularised procedures
- Removed -B option: Bus info is now enabled at all times, as bus info is now at least as important as basic device ID, on modern hardware.
- Updated PCI Express Capability report layout for better debugging/readability
- Updated CardBus support with much more decoding of CardBus Bridge data

Version 1.4
- Fixed a bug with VPD dumps in -Z (debug) mode
- Fixed a bug with -R reporting wrong item count if an empty bus was encountered
- Fixed some typos and indent formatting errors
- Fixed big bug with Wrong SubSystem info reported in certain cases
- Fixed -R option to report CardBus busses, even if unpopulated
- Added support for new Hi Definition Audio class code
- Re-write of workoutbusses routine. Now supports multiple root-bus systems, which was broken in v1.3
- Now supports up to 512 actual devices (was 200 devices previously)
