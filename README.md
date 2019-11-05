# Elm-code
Assignments and exercises coded in Elm.

To compile and run any of the modules:

1. Install [Elm](https://guide.elm-lang.org/install/elm.html).
2. Open your ```cmd``` or ```powershell``` and create a directory at any chosen directory path:
```
mkdir <project_folder_name>
```
3. Change your working directory:
```
cd <project_folder_name>
```
4. To create an elm project in this directory, type:
```
elm init
```
This will elm.json file and a src/ directory.

**Note**: If elm is an unrecognized command, make sure that the path to your Elm executable is properly appended to your Path environment variable. 

5. Run the following command to install the Graphics SVG library:
```
elm install MacCASOutreach/graphicsvg
```

Documentation for this package can be found [here](https://package.elm-lang.org/packages/MacCASOutreach/graphicsvg/6.1.0/GraphicSVG
).

6. If your elm.json file and src/ directory were properly created in your project folder, copy any of the .elm modules into the src/ folder. 
7. From the project folder path in the shell (..\<project_folder_name>), run the following command to compile and build the files in src/:
```
elm reactor
```
8. Navigate over to the following server to check out the compiled module: http://localhost:8000.
