---
title:                "Creating a temporary file"
html_title:           "C++ recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
Temporary files are useful for storing data temporarily in a program, without cluttering up the main file or database. They can be easily created and deleted as needed, providing a temporary storage solution for various tasks.

## How To
Creating a temporary file in C++ is a simple process that involves using the ```tmpnam()``` function from the standard library. Here's an example code to illustrate the process:

```C++
#include <iostream>
#include <cstdio> //for tmpnam()

using namespace std;

int main() {
    char *filename; //variable to store temporary file name
    filename = tmpnam(NULL); //passing NULL as argument generates temporary file name
    cout << "Temporary file created: " << filename << endl;

    //open the temporary file for writing
    FILE *file = fopen(filename, "w");
    if (file != NULL) {
        fprintf(file, "This is a temporary file.");
        fclose(file);
        cout << "Data successfully written to temporary file." << endl;
    } else {
        cout << "Error opening file." << endl;
    }

    //now delete the temporary file
    if (remove(filename) == 0) {
        cout << "Temporary file deleted." << endl;
    } else {
        cout << "Error deleting file." << endl;
    }
    
    return 0;
}
```

This code will create a temporary file, write some data to it, and then delete the file once it is no longer needed. Here's a sample output:

```
Temporary file created: C:\Users\John\AppData\Local\Temp\tmp.1MnN0D
Data successfully written to temporary file.
Temporary file deleted.
```

## Deep Dive
The ```tmpnam()``` function generates a unique file name each time it is called, ensuring that the temporary file created will not overwrite any existing files. However, this does not guarantee that the file won't be overwritten by another program. Additionally, the temporary file may not always be created in the designated temporary directory. It is important to check for failure when creating or deleting temporary files.

## See Also
- [C++ Standard Library tmpnam()](https://www.cplusplus.com/reference/cstdio/tmpnam/)
- [Creating and Deleting Temporary Files in C++](http://www.math.uaa.alaska.edu/~afkjm/csce211/handouts/TemporaryFiles.pdf)
- [Understanding C and C++ File IO](https://www3.ntu.edu.sg/home/ehchua/programming/cpp/c1_FileIO.html)