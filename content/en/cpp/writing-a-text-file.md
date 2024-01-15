---
title:                "Writing a text file"
html_title:           "C++ recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file is a common task in programming, especially in the context of file manipulation and data storage. It allows for the easy organization and storage of large amounts of text data in a computer-readable format.

## How To

Writing a text file in C++ is a relatively straightforward process. It involves the following steps:

1. First, we need to include the necessary header file for file handling in C++, ```<fstream>```.

2. Next, we need to create an output stream object using the ```ofstream``` class. This object will represent the file that we want to write to.

    ```C++
    ofstream outputFile;

    outputFile.open("my_file.txt"); // creates or overrides existing file named "my_file.txt"
    ```

3. Now, we can use the ```<<``` operator to write data to the file. This operator is commonly used for outputting data to streams in C++. We can also use the ```write()``` function for more precise control over the data being written.

    ```C++
    outputFile << "This is a sample text file." << endl;
    outputFile.write("Some more text", 12); // writes the first 12 characters of "Some more text" to the file
    ```

4. Once we have finished writing to the file, we need to close the output stream object to ensure that all the data has been successfully written to the file.

    ```C++
    outputFile.close();
    ```

5. Upon running the code, a new file named "my_file.txt" will be created (if it does not already exist) and the specified data will be written to it.

Sample Output in "my_file.txt":

<<<<<<< HEAD
```C++
#include <iostream>
#include <fstream>

int main() {
  // Create an instance of ofstream
  std::ofstream file("output.txt", std::ios::out);
  
  if(file.is_open()) {
    // Write to file using the insertion operator
    file << "Hello world!" << std::endl;
    file << "This is a sample text file." << std::endl;
    
    // Close the file
    file.close();
    
    // Output success message
    std::cout << "Text file successfully written." << std::endl;
  }
  else {
    std::cout << "Error opening file." << std::endl;
  }
  
  return 0;
}
=======
```
This is a sample text file.
Some more text
>>>>>>> ca53d9f8 (better writing, new default title)
```

## Deep Dive

There are a few important things to keep in mind when writing a text file in C++. Firstly, the ```open()``` function can take in an additional parameter to specify the file's writing mode. For example, using ```ios::app``` will append the data to the end of the existing file instead of overriding it.

Secondly, it is best practice to check if the file has been successfully opened before proceeding with the writing process. This can be done by using the ```is_open()``` function.

Thirdly, we can also use the ```tellp()``` function to get the current position of the file pointer, which can be useful for seeking to specific locations in the file for writing.

Lastly, it is important to handle any potential errors that may occur during the writing process. This can be done using the ```fail()``` function and checking for any errors before proceeding.

## See Also

<<<<<<< HEAD
- [C++ file handling tutorial](https://www.programiz.com/cpp-programming/files-input-output)
- [UTF-8 encoding](https://www.w3schools.com/charsets/ref_utf_basic_latin.asp)
=======
- [C++ File Handling](https://www.geeksforgeeks.org/file-handling-c-classes/) - A comprehensive guide on file handling in C++ from GeeksforGeeks.
- [Writing Text Files in C++](https://www.learncpp.com/cpp-tutorial/185-file-io/) - A tutorial on writing text files in C++ from LearnCPP.com.
>>>>>>> ca53d9f8 (better writing, new default title)
