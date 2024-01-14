---
title:    "Python recipe: Creating a temporary file"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why 

Creating temporary files is a common practice in Python programming when working with large amounts of data. Temporary files provide a way to store information temporarily, without cluttering the main code, and can be easily deleted after use. They are especially useful for intermediate processing steps or when sharing data between different parts of the code. 

## How To

To create a temporary file in Python, we can use the built-in module "tempfile". The "NamedTemporaryFile" function creates a named temporary file and stores it in the directory provided (or in the default temporary directory if none is specified). We can then write data to the file using the "write" function, and close it when we're finished. Here's an example:

```Python
import tempfile 

# Creating a temporary file named "temp_file"
with tempfile.NamedTemporaryFile(suffix='.txt') as temp_file: 
  # Writing data to the file 
  temp_file.write(b'Hello World!') 
  # Getting the file name 
  print('Temporary file created:', temp_file.name) 
  # Closing the file 
  temp_file.close() 
```

**Output:** Temporary file created: C:/Users/username/AppData/Local/Temp/tmp_P6v5.txt 

Note that the file is automatically deleted after the "with" statement ends. We can also specify a prefix for the temporary file using the "prefix" argument in the function. 

## Deep Dive 

The "NamedTemporaryFile" function accepts several other arguments that allow us to customize the creation of the temporary file. The "mode" argument specifies the file mode, which by default is set to "w+b" (write and binary mode). We can also specify the "dir" argument to store the file in a different directory. If we want to keep the temporary file even after the program ends, we can use the "delete" argument and set it to "False". 

Additionally, we can use the "TemporaryFile" function to create a temporary file without specifying a name, which generates a random name for the file. This can be useful if we don't need to access the file by name. 

## See Also 

- [Python documentation for tempfile module](https://docs.python.org/3/library/tempfile.html)
- [RealPython tutorial on temporary files in Python](https://realpython.com/read-write-files-python/)
- [GeeksforGeeks article on tempfile module](https://www.geeksforgeeks.org/temporary-files-python/)