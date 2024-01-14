---
title:    "C++ recipe: Creating a temporary file"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
Creating temporary files can be useful when working with large amounts of data or when temporarily storing information. They can also be helpful for managing memory and organizing code.

## How To
To create a temporary file in C++, we can use the ```tmpfile()``` function from the ```<cstdio>``` header. This function takes no arguments and returns a ```FILE*``` pointer to the newly created temporary file.

```C++
#include <cstdio>
#include <iostream>

int main() {
  FILE* temp_file = tmpfile();
  if (temp_file != NULL) {
    std::cout << "Temporary file created successfully!" << std::endl;
    // use the temporary file here
    fclose(temp_file); // close the file when finished
  } else {
    std::cout << "Error creating temporary file." << std::endl;
  }
  return 0;
}
```

This code first includes the appropriate headers, then uses the ```tmpfile()``` function to create a temporary file and stores the returned pointer in the ```temp_file``` variable. We then use a check to see if the file was successfully created and use it accordingly. Finally, we use the ```fclose()``` function to close the file when we are finished using it.

The output of the code above would be:
```
Temporary file created successfully!
```

## Deep Dive
The ```tmpfile()``` function creates a temporary file with a unique name in the default temporary directory for the system. This means that the file names are not predictable and are different for each execution of the program. These files are also automatically deleted when they are closed or when the program terminates, making them perfect for temporary storage.

It's important to note that the returned ```FILE*``` pointer points to an open file stream, so it's necessary to close the file when it is no longer needed. This also ensures that any data written to the file is properly saved.

## See Also
- [C++ File Input/Output](https://www.programiz.com/cpp-programming/files-input-output)
- [Understanding tmpfile() in C++](https://www.geeksforgeeks.org/tmpfile-function-in-c-cpp/)
- [Working with Temporary Files in C++](https://www.techiedelight.com/creating-temporary-file-cpp/)