;;; jde-autoload.el -- Integrated Development Environment for Java.
;; $Revision: 1.9 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2000, 2001, 2002 Paul Kinnucan.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Gnu Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;; Commentary:

;; This file is intended to decrease the startup time for the JDEE by
;; delaying the loading of many JDEE packages until they are used for
;; the first time in a session.

;; Please note that this file is automatically generated by 
;; jde-autoload-update.


;;;### (autoloads (jde-compile-jde jde-show-help jde-create-new-project
;;;;;;  jde-save-project jde-open-project-file jde-mode jde-build
;;;;;;  jde-set-global-classpath) "jde" "jde.el" (15882 42278))
;;; Generated autoloads from jde.el

(defconst jde-version "2.3.2" "\
JDE version number.")

(autoload (quote jde-set-global-classpath) "jde" "\
Set the value of `jde-global-classpath'.
It specifies the -classpath argument for the Java compiler and
interpreter." t nil)

(autoload (quote jde-build) "jde" "\
Rebuild the entire project.
This command invokes the function defined by `jde-build-function'." t nil)

(autoload (quote jde-mode) "jde" "\
Major mode for developing Java applications and applets.
\\{jde-mode-map}" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.java\\'" . jde-mode)))

(autoload (quote jde-open-project-file) "jde" "\
Opens the project file for the Java source file in the
current buffer." t nil)

(autoload (quote jde-save-project) "jde" "\
Saves source file buffer options in one or more project files.
This command provides an easy way to create and update a project file
for a Java project. Simply open a source file, set the desired
options, using the JDE Options menu, then save the settings in the
project file, using this command.  Now, whenever you open a source
file from the same directory tree, the saved settings will be restored
for that file." t nil)

(autoload (quote jde-create-new-project) "jde" "\
Creates a new JDE project file in directory NEW-DIR, saving any
current customized variables.  If a project file already exists in the
given directory, the project is simply re-saved.  This functions the
same as `jde-save-project' when no project files can be found for the
current source file.  But, if there already exist projects somewhere
along the path, this command unconditionally creates a project file in
the directory specified, thus allowing the user to create and maintain
hierarchical projects." t nil)

(autoload (quote jde-show-help) "jde" "\
Displays the JDE User's Guide in a browser." t nil)

(autoload (quote jde-compile-jde) "jde" "\
Byte-compile all uncompiled files of jde." t nil)

;;;***

;;;### (autoloads (jde-ant-show-options jde-ant-projecthelp jde-ant-build)
;;;;;;  "jde-ant" "jde-ant.el" (15869 31622))
;;; Generated autoloads from jde-ant.el

(autoload (quote jde-ant-build) "jde-ant" "\
Build the current project using Ant.  If interactive, we try to prompt the
  user for certain variables.." t nil)

(autoload (quote jde-ant-projecthelp) "jde-ant" "\
Display Ant project help for the current project.
This will execute the Ant program with the `-projecthelp' switch to output
available targets with their descriptions for the current buildfile. This
function uses the same rules as `jde-ant-build' for finding the buildfile." t nil)

(autoload (quote jde-ant-show-options) "jde-ant" "\
Show the JDE Ant Options panel." t nil)

;;;***

;;;### (autoloads (jde-bug-debug-app) "jde-bug" "jde-bug.el" (15874
;;;;;;  35894))
;;; Generated autoloads from jde-bug.el

(autoload (quote jde-bug-debug-app) "jde-bug" "\
Runs the debugger on the application in the current source buffer." t nil)

;;;***

;;;### (autoloads (jde-checkstyle jde-checkstyle-customize) "jde-check"
;;;;;;  "jde-check.el" (15724 22822))
;;; Generated autoloads from jde-check.el

(autoload (quote jde-checkstyle-customize) "jde-check" "\
Customization of the group jde-checkstyle." t nil)

(autoload (quote jde-checkstyle) "jde-check" "\
Checks the Java program in the current buffer.
This command invokes the style checker specified by `jde-checkstyle-class'
with the options specified by the JDEE customization variables
that begin with `jde-checkstyle'. If the variable
`jde-read-checkstyle-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled." t nil)

;;;***

;;;### (autoloads (jde-checkstyle jde-checkstyle-customize) "jde-checkstyle"
;;;;;;  "jde-checkstyle.el" (15876 12046))
;;; Generated autoloads from jde-checkstyle.el

(autoload (quote jde-checkstyle-customize) "jde-checkstyle" "\
Set Java style checking options." t nil)

(autoload (quote jde-checkstyle) "jde-checkstyle" "\
Checks the Java program in the current buffer.
This command invokes the style checker specified by `jde-checkstyle-class'
with the options specif2ied by the JDEE customization variables
that begin with `jde-checkstyle'. If the variable
`jde-read-checkstyle-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled." t nil)

;;;***

;;;### (autoloads (jde-compile jde-set-compile-options) "jde-compile"
;;;;;;  "jde-compile.el" (15864 7582))
;;; Generated autoloads from jde-compile.el

(autoload (quote jde-set-compile-options) "jde-compile" "\
Sets the compile options.
Enter the options as you would on the command line, e.g.,
-depend -verbose." t nil)

(autoload (quote jde-compile) "jde-compile" "\
Compile the Java program in the current buffer.
This command invokes the compiler specified by `jde-compiler'
with the options specified by the JDE customization variables
that begin with `jde-compile'. If the variable
`jde-read-compile-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled. If `jde-compiler' specifies the JDE compile
server, this command uses the compile server. Otherwise, it
uses the compiler executable specified by
`jde-compiler' to compile." t nil)

;;;***

;;;### (autoloads (jde-db-set-app-args jde-db-set-args jde-db-set-debugger)
;;;;;;  "jde-db" "jde-db.el" (15807 55248))
;;; Generated autoloads from jde-db.el

(autoload (quote jde-db-set-debugger) "jde-db" "\
Specify the pathname of the debugger, if an executable, or the
debugger's fully qualified class name, if a class." t nil)

(autoload (quote jde-db-set-args) "jde-db" "\
Specify the arguments (except -classpath) to be passed to the debugger." t nil)

(autoload (quote jde-db-set-app-args) "jde-db" "\
Specify the arguments to be passed to the Java application class." t nil)

;;;***

;;;### (autoloads (jde-ejb-entity-bean-buffer jde-ejb-session-bean-buffer)
;;;;;;  "jde-ejb" "jde-ejb.el" (15852 14170))
;;; Generated autoloads from jde-ejb.el

(autoload (quote jde-ejb-session-bean-buffer) "jde-ejb" "\
Create a new Java buffer containing an EJB session bean class of the same name.
This command also creates buffers with the EJB Home and EJB Remote interfaces
and the XML Deployment descriptor defined
by the jde-ejb templates.  This includes naming the files according 
to the EJB naming convention." t nil)

(autoload (quote jde-ejb-entity-bean-buffer) "jde-ejb" "\
Create a new Java buffer containing an EJB entity bean class of the same name.
This command also creates buffers with the EJB Home and EJB Remote interfaces
and the XML Deployment descriptor defined
by the jde-ejb templates.  This includes naming the files according 
to the EJB naming convention." t nil)

;;;***

;;;### (autoloads (jde-gen-junit-test-class-buffer jde-gen-buffer
;;;;;;  jde-gen-jfc-app-buffer jde-gen-console-buffer jde-gen-interface-buffer
;;;;;;  jde-gen-class-buffer) "jde-gen" "jde-gen.el" (15873 26832))
;;; Generated autoloads from jde-gen.el

(autoload (quote jde-gen-class-buffer) "jde-gen" "\
Create a new Java buffer containing a class of the same name.
This command inserts the class template generated by `jde-gen-class'.
It then moves the point to the location to the constructor." t nil)

(autoload (quote jde-gen-interface-buffer) "jde-gen" "\
Create a new Java buffer containing an interface of the same name.
This command inserts the interface template generated by `jde-gen-interface'.
It then moves the point to the location of the first method." t nil)

(autoload (quote jde-gen-console-buffer) "jde-gen" "\
Create a new Java buffer containing a console class of the same name.
This command inserts the class template generated by `jde-gen-class'.
It then moves the point to the location to the constructor." t nil)

(autoload (quote jde-gen-jfc-app-buffer) "jde-gen" "\
Create a new Java buffer containing a JFC application class.
This command inserts the class template generated by `jde-gen-jfc-app'.
It then moves the point to the location to the constructor." t nil)

(autoload (quote jde-gen-buffer) "jde-gen" "\
Create a new Java buffer containing a code template.
This command inserts the specified template at the beginning
of the buffer." t nil)

(autoload (quote jde-gen-junit-test-class-buffer) "jde-gen" "\
Create a buffer containing a skeleton unit test class having the same name as the
root name of the buffer. This command prompts you to enter the file name
of the test class. It assumes that the file name has the form CLASSTest.java
where CLASS is the name of the class to be tested, e.g., MyAppTest.java. Use 
`jde-gen-junit-add-test-to-suite' to add tests to the test suite. Use of
tests generated with this template requires the JUnit test framework. For
more information, see http://www.junit.org." t nil)

;;;***

;;;### (autoloads (jde-gen-junit-test-class-buffer jde-gen-buffer
;;;;;;  jde-gen-jfc-app-buffer jde-gen-console-buffer jde-gen-interface-buffer
;;;;;;  jde-gen-class-buffer) "jde-gen1" "jde-gen1.el" (15634 54722))
;;; Generated autoloads from jde-gen1.el

(autoload (quote jde-gen-class-buffer) "jde-gen1" "\
Create a new Java buffer containing a class of the same name.
This command inserts the class template generated by `jde-gen-class'.
It then moves the point to the location to the constructor." t nil)

(autoload (quote jde-gen-interface-buffer) "jde-gen1" "\
Create a new Java buffer containing an interface of the same name.
This command inserts the interface template generated by `jde-gen-interface'.
It then moves the point to the location of the first method." t nil)

(autoload (quote jde-gen-console-buffer) "jde-gen1" "\
Create a new Java buffer containing a console class of the same name.
This command inserts the class template generated by `jde-gen-class'.
It then moves the point to the location to the constructor." t nil)

(autoload (quote jde-gen-jfc-app-buffer) "jde-gen1" "\
Create a new Java buffer containing a JFC application class.
This command inserts the class template generated by `jde-gen-jfc-app'.
It then moves the point to the location to the constructor." t nil)

(autoload (quote jde-gen-buffer) "jde-gen1" "\
Create a new Java buffer containing a code template.
This command inserts the specified template at the beginning
of the buffer." t nil)

(autoload (quote jde-gen-junit-test-class-buffer) "jde-gen1" "\
Create a new Java buffer containing a class of the same name.
This command inserts the class template generated by `jde-gen-junit-test-class'.
It then moves the point to the location to the constructor." t nil)

;;;***

;;;### (autoloads (jde-help-browse-jdk-doc) "jde-help" "jde-help.el"
;;;;;;  (15866 46394))
;;; Generated autoloads from jde-help.el

(autoload (quote jde-help-browse-jdk-doc) "jde-help" "\
Displays the JDK doc in a web browser. This function uses the URL
stored in the variable jde-jdk-doc-url to locate the JDK documentation." t nil)

;;;***

;;;### (autoloads (jde-import-organize) "jde-import" "jde-import.el"
;;;;;;  (15741 33436))
;;; Generated autoloads from jde-import.el

(autoload (quote jde-import-organize) "jde-import" "\
Organize import statements of the current Java source buffer.
If optional FORCE is non-nil force reordering even if imports are
already organized.

Imports are organized into groups returned by the function specified
by `jde-import-group-function'.  Groups are inserted in the order they
are found unless `jde-import-sorted-groups' requires that they must be
alphabetically sorted.  In each group imports are sorted by name
alphabetically or in reverse order if `jde-import-reverse-sort-group'
is non-nil.  A blank line is inserted between groups.

Usage:
  \\[jde-import-organize] group and sort import statements.
  \\[universal-argument] \\[jde-import-organize] to force reordering.

The current buffer must be in `jde-mode'.  This command requires a
version of the JDE with the semantic parser." t nil)

;;;***

;;;### (autoloads (jde-java-font-lock-setup-keywords) "jde-java-font-lock"
;;;;;;  "jde-java-font-lock.el" (15749 28510))
;;; Generated autoloads from jde-java-font-lock.el

(autoload (quote jde-java-font-lock-setup-keywords) "jde-java-font-lock" "\
Setup font lock keywords in `java-font-lock-keywords-4'.
If optional REBUILD flag is non-nil create a new cache of regular
expressions." t nil)

;;;***

;;;### (autoloads (jde-javadoc-enable-menu-p jde-javadoc-checkdoc
;;;;;;  jde-javadoc-checkdoc-at-line jde-javadoc-autodoc-at-line
;;;;;;  jde-javadoc-customize jde-javadoc-checker-quit jde-javadoc-checker-fix
;;;;;;  jde-javadoc-checker-next jde-javadoc-checker-previous) "jde-javadoc"
;;;;;;  "jde-javadoc.el" (15885 19030))
;;; Generated autoloads from jde-javadoc.el

(autoload (quote jde-javadoc-checker-previous) "jde-javadoc" "\
Go to the previous token with doc errors." t nil)

(autoload (quote jde-javadoc-checker-next) "jde-javadoc" "\
Goto the next token with doc errors." t nil)

(autoload (quote jde-javadoc-checker-fix) "jde-javadoc" "\
Fix documentation of checked token.
Used in `jde-javadoc-checker-report-mode'." t nil)

(autoload (quote jde-javadoc-checker-quit) "jde-javadoc" "\
Quit the `jde-javadoc-checker' report buffer.
Used in `jde-javadoc-checker-report-mode'." t nil)

(autoload (quote jde-javadoc-customize) "jde-javadoc" "\
Show the jde-javadoc options panel." t nil)

(autoload (quote jde-javadoc-autodoc-at-line) "jde-javadoc" "\
Update javadoc comment block for declaration at current line.

Uses the semantic bovinator parser table to find declarations (see
`jde-javadoc-nonterminal-at-line').

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION.  IF NOT RESULT IS UNCERTAIN.

In the following examples, point is located at the beginning of the
line, before the word 'public' (but it could be anywhere on this
line):

1- Class example:
   -------------

-|-  public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

\\[jde-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe class <code>MyClass</code> here.
+     *
+     * @author \"David Ponce\" <david.ponce@wanadoo.fr>
+     * @version 1.0
+     * @since 1.0
+     * @see MySuperClass
+     * @see Runnable
+     * @see java.io.Serializable
+     */
     public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

2- Method example:
   --------------

-|-  public
     void   myMethod( int  x,  int y )
       throws Exception
     {
       ...

\\[jde-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe <code>myMethod</code> method here.
+     *
+     * @param x an <code>int</code> value
+     * @param y a <code>long</code> value
+     * @exception Exception if an error occurs
+     */
     public
     void   myMethod( int  x,  long y )
       throws Exception
     {
       ...

3- Field example:
   --------------

-|-  private static final int SIZE = 10;

\\[jde-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe constant <code>SIZE</code> here.
+     */
     private static final int SIZE = 10;


`tempo' templates are used for each category of javadoc line.  The
following templates are currently defined and fully customizable (see
`tempo-define-template' for the different items that can be used in a
tempo template):

- - `jde-javadoc-author-tag-template'
- - `jde-javadoc-describe-class-template'
- - `jde-javadoc-describe-constructor-template'
- - `jde-javadoc-describe-interface-template'
- - `jde-javadoc-describe-method-template'
- - `jde-javadoc-describe-field-template'
- - `jde-javadoc-exception-tag-template'
- - `jde-javadoc-param-tag-template'
- - `jde-javadoc-return-tag-template'
- - `jde-javadoc-version-tag-template'

For example if you customize `jde-javadoc-describe-class-template'
with the following value:

'(\"* \" (P \"Class description: \"))

you will be asked to enter the class description in the minibuffer.
See also the `jde-javadoc-field-type', `jde-javadoc-a' and
`jde-javadoc-code' helper functions." t nil)

(autoload (quote jde-javadoc-checkdoc-at-line) "jde-javadoc" "\
Check javadoc comment block of declaration at current line.

Uses the semantic bovinator parser table to find declarations (see
`jde-javadoc-nonterminal-at-line').

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION.  IF NOT RESULT IS UNCERTAIN." t nil)

(autoload (quote jde-javadoc-checkdoc) "jde-javadoc" "\
Check doc comments of tokens in the current buffer.
Report the next token with documentation errors." t nil)

(autoload (quote jde-javadoc-enable-menu-p) "jde-javadoc" "\
Return non-nil if corresponding doc menu item is enabled.
That is point is on the first line of a class, method, or field
definition." nil nil)

;;;***

;;;### (autoloads (jde-javadoc-make) "jde-javadoc-gen" "jde-javadoc-gen.el"
;;;;;;  (15887 54074))
;;; Generated autoloads from jde-javadoc-gen.el

(autoload (quote jde-javadoc-make) "jde-javadoc-gen" "\
Generates javadoc for the current project. This command runs the
JDE javadoc program to generate the documentation. The variable
`jde-javadoc-command-path' specifies the path of the javadoc excutable.
The variable `jde-global-classpath' specifies the javadoc 
-classpath argument. The variable `jde-sourcepath'
specifies the javadoc  -sourcepath argument. You can specify all
other javadoc options via JDE customization variables. To specify the
options, select Project->Options->Javadoc from the JDE menu. Use 
`jde-javadoc-gen-packages' to specify the packages, classes, or source
files for which you want to generate javadoc. If this variable is nil,
this command generates javadoc for the Java source file in the current
buffer. If `jde-javadoc-display-doc' is nonnil, this command displays
the generated documentation in a browser." t nil)

;;;***

;;;### (autoloads (jde-jdb-applet jde-jdb) "jde-jdb" "jde-jdb.el"
;;;;;;  (15823 16138))
;;; Generated autoloads from jde-jdb.el

(autoload (quote jde-jdb) "jde-jdb" "\
Run jdb on Java application whose source resides in the current buffer.
This command determines the main class of the application either from
the variable `jde-run-application-class' or from the source in the current 
buffer. If `jde-run-application-class' does not specify a class, the main class
is assumed to be the class defined by the current source buffer. This command
creates a command buffer for the debug session." t nil)

(autoload (quote jde-jdb-applet) "jde-jdb" "\
Runs an applet in the jdb debugger. This function prompts you to enter
the path of an html document that displays the applet. If you 
do not enter a path, this function next checks
whether `jde-run-applet-doc' specifies a document. If so, it displays
that specified document. Next, it checks whether the current directory
contains any html files. If so, it displays the first html file that
it finds. If if cannot find an html file, it signals an error.  This
function runs appletviewer in jdb to permit debugging. On startup, it
sets a breakpoint in the init method of the class specified by 
`jde-run-application-class' or in the class corresponding to the Java
file in the current buffer." t nil)

;;;***

;;;### (autoloads (jde-make-show-options jde-make) "jde-make" "jde-make.el"
;;;;;;  (15836 24226))
;;; Generated autoloads from jde-make.el

(autoload (quote jde-make) "jde-make" "\
Run the make program specified by `jde-make-program' with the
command-line arguments specified by `jde-make-args'. If
`jde-read-make-args' is nonnil, this command also prompts you to enter
make arguments in the minibuffer and passes any arguments that you
enter to the make program along with the arguments specified by
`jde-make-args'." t nil)

(autoload (quote jde-make-show-options) "jde-make" "\
Show the JDE Make Options panel." t nil)

;;;***

;;;### (autoloads (jde-package-update) "jde-package" "jde-package.el"
;;;;;;  (15873 27056))
;;; Generated autoloads from jde-package.el

(autoload (quote jde-package-update) "jde-package" "\
Create or update the package statement in the current Java source
file buffer based on the file's location relative to the root of
the package directory as specified by one of the entries in
`jde-package-search-classpath-variables' or `jde-sourcepath'.
If these variables do not specify the root of the package directory,
this command does nothing. This command signals an error if the
 major mode of the current buffer is not `jde-mode'." t nil)

;;;***

;;;### (autoloads (jde-run-applet jde-run jde-run-set-applet-doc
;;;;;;  jde-run-set-applet-viewer jde-run-set-app-args jde-run-set-args
;;;;;;  jde-run-set-app) "jde-run" "jde-run.el" (15848 10334))
;;; Generated autoloads from jde-run.el

(autoload (quote jde-run-set-app) "jde-run" "\
Specify the name of the application class to run." t nil)

(autoload (quote jde-run-set-args) "jde-run" "\
Specify arguments to be passed to the Java vm.
This command serves as an alternative to using the JDE Run Options
panel to specify command-line arguments for the Java interpreter." t nil)

(autoload (quote jde-run-set-app-args) "jde-run" "\
Specify the arguments to be passed to the Java application class.
This command provides an alternative to using the JDE Run Options panel
to specify command-line arguments to pass to the application when starting
the application." t nil)

(autoload (quote jde-run-set-applet-viewer) "jde-run" "\
Sets the viewer to be used to view an applet. The default is 
appletviewer." t nil)

(autoload (quote jde-run-set-applet-doc) "jde-run" "\
Specify the doc to be used to view an applet.
This command provides an alternative to using the JDE Options
panel to specifying the applet document." t nil)

(autoload (quote jde-run) "jde-run" "\
Run the Java application specified by `jde-run-executable', if
not the null string. Otherwise run the class specified by 
`jde-run-application-class', if non-null; otherwise the class in
the current buffer. Specifying a prefix argument, e.g.,
C-u C-c C-v C-r, causes this command to prompt you to enter
the name of the application's main class This command 
creates a comint buffer to allow you to interact with the program." t nil)

(autoload (quote jde-run-applet) "jde-run" "\
Runs an applet. This function prompts you to enter the path of an
html document that displays the applet. If you enter return without
specifying a document, this function next checks whether
`jde-run-applet-doc' specifies a document. If so, it displays that
specified document. Next, it checks whether the current directory
contains any html files. If the current directory contains an html
file with the same root name as the Java file in the current buffer,
it displays the file. If not, it displays the first html file that it
finds in the current directory. If if cannot find an html file, it
signals an error.  This function uses the viewer specified by
`jde-run-applet-viewer' to display the specified document. Note that
if you run two html applet files successively with the same name, you
must kill the buffer created to run the first file before running the
second file. Otherwise, this command will simply redisplay the first
file." t nil)

;;;***

;;;### (autoloads (jde-stat-loc-report-directory jde-stat-loc-report-project
;;;;;;  jde-stat-loc-report) "jde-stat" "jde-stat.el" (15836 24226))
;;; Generated autoloads from jde-stat.el

(autoload (quote jde-stat-loc-report) "jde-stat" "\
Generates a report showing the number of code, comment,
javadoc, and blank lines in the current Java source buffer. Optionally
a total count could be passed to be displayes, as well as the number of
processed files." t nil)

(autoload (quote jde-stat-loc-report-project) "jde-stat" "\
Generates a report showing the number of code, comment,
javadoc, and blank lines in all the java files in the current
directory and subdirectories. This method will kill any
buffer containing a java file contained in dir." t nil)

(autoload (quote jde-stat-loc-report-directory) "jde-stat" "\
Generates a report showing the number of code, comment,
javadoc, and blank lines in all the java files in the current
directory. This method will kill any buffer containing a java file
contained in dir." t nil)

;;;***

;;;### (autoloads (jde-which-method-mode) "jde-which-method" "jde-which-method.el"
;;;;;;  (15035 4342))
;;; Generated autoloads from jde-which-method.el

(defvar jde-which-method-mode t "\
Enables the JDE's which method mode.
When which method mode is enabled, the current method name is
displayed in the mode line.")

;;;***

;;;### (autoloads (jde-xref-customize jde-xref-update jde-xref-list-uncalled-functions
;;;;;;  jde-xref-display-call-tree jde-xref-next-caller jde-xref-first-caller
;;;;;;  jde-xref-make-xref-db) "jde-xref" "jde-xref.el" (15873 25354))
;;; Generated autoloads from jde-xref.el

(autoload (quote jde-xref-make-xref-db) "jde-xref" "\
Create a database of caller to callee (and the reverse) from the
classes in `jde-xref-class-path' and store the data in the location
specified by `jde-xref-db-file'" t nil)

(autoload (quote jde-xref-first-caller) "jde-xref" "\
Put the list of who calls the current function on the stack and
display the first caller.  Subsequent callers are displayed through
`jde-xref-show-next-caller'.  STRICT should be true if the callers of
interfaces to a function should not be considered" t nil)

(autoload (quote jde-xref-next-caller) "jde-xref" "\
If there are items still on the caller stack, pop the first one off
and show it" t nil)

(autoload (quote jde-xref-display-call-tree) "jde-xref" "\
Display an interactive call tree of which function call the current
  function, which can be expanded outward.  STRICT should be true if
  the callers of interfaces to a function should not be considered" t nil)

(autoload (quote jde-xref-list-uncalled-functions) "jde-xref" "\
Displays a simple list of function that are never called, at least
not directly.  Do not assume that this means this code can never be
reached, since reflection could always call any method.  Use this list
and your best judgement to figure out what are good candidates for
code cleanup.  STRICT should be set to true if the callers of
interfaces to a function should not be considered.  This function could
take a while. If it does, you might want to consider increasing
`jde-xref-cache-size'." t nil)

(autoload (quote jde-xref-update) "jde-xref" "\
Update the caller table after a recompile.  This can be called by
the user when they recompile outside of emacs.  It will update the
call list of all files modified in emacs" t nil)

(autoload (quote jde-xref-customize) "jde-xref" "\
Display the customization buffer for the xref package." t nil)

;;;***

;;;### (autoloads (bsh-script-help) "beanshell" "beanshell.el" (15877
;;;;;;  24288))
;;; Generated autoloads from beanshell.el

(autoload (quote bsh-script-help) "beanshell" "\
Display BeanShell User's Guide." t nil)

;;;***

(provide 'jde-autoload)

;; end of jde-autoload.el