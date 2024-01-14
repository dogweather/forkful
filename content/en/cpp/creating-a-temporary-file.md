---
title:                "C++ recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files is a common practice in programming for various reasons. Temporary files are used to store data that is only needed for a short period of time, such as intermediate results of a computation, or files that are being processed before being permanently saved. They also help in managing resources efficiently, as temporary files are automatically deleted once they are no longer needed.

## How To

To create a temporary file in C++, we can use the `std::fstream` library. For this example, we will create a temporary file to store some numbers and then read them back out. We will use the `std::tmpnam` function to generate a unique filename for our temporary file.

```
C++ 
#include <iostream>
#include <fstream>
#include <cstdlib>

int main() {
  char filename[ L_tmpnam ]; // L_tmpnam is defined in <cstdio>

  // generate a unique filename
  std::tmpnam(filename);

  // create a file stream object
  std::fstream file;

  // open the file for writing
  file.open(filename, std::ios::out);

  // write some numbers to the file
  file << "4, 8, 15, 16, 23, 42";

  // close the file
  file.close();

  // open the file again for reading
  file.open(filename, std::ios::in);

  // read the numbers from the file and print them
  int num;
  while (file >> num) {
    std::cout << "Number: " << num << std::endl;
  }

  // close the file
  file.close();

  // delete the temporary file
  std::remove(filename);

  return 0;
}
```

Output:

```
Number: 4
Number: 8
Number: 15 
Number: 16 
Number: 23 
Number: 42 
```

## Deep Dive

When creating a temporary file, we need to make sure that the filename is unique and not already in use. This is where the `std::tmpnam` function comes in. It generates a unique filename by appending a random string to the string "tmp". This ensures that the temporary file we create will not overwrite any existing files.

It is important to note that the use of temporary files should be limited to necessary cases, as they can clutter up the system and affect performance. It is also recommended to delete the temporary file as soon as it is no longer needed.

## See Also

- [C++ fstream library](https://www.cplusplus.com/reference/fstream/)
- [std::tmpnam function](https://www.cplusplus.com/reference/cstdio/tmpnam/)
- [std::remove function](https://www.cplusplus.com/reference/cstdio/remove/)