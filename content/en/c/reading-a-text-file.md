---
title:                "Reading a text file"
date:                  2024-01-20T17:53:39.695960-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading a text file"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file is about accessing the file's data as string content, character by character or line by line. Programmers do it to process, analyze, or manipulate stored information without manual input every run.

## How to:

Let's read a text file. We'll open it, read from it, and close it. Basic stuff.

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file;
    char filename[] = "example.txt";
    char ch;

    file = fopen(filename, "r"); // Open the file in read mode

    if (file == NULL) {
        perror("Error while opening the file.\n");
        exit(EXIT_FAILURE);
    }

    printf("Contents of %s:\n", filename);

    while ((ch = fgetc(file)) != EOF) { // Read and print char by char
        putchar(ch);
    }

    fclose(file); // Close the file

    return 0;
}
```

Assuming `example.txt` holds "Hello, C!", output will be:
```
Contents of example.txt:
Hello, C!
```

## Deep Dive

Back in the 70s, C was born, and with it, the way we read files today. It's not rocket science, but there are nuances. You use `fopen` to open files and `fgetc` to read one character at a time. But why char by char? You could read lines with `fgets` or the whole file with `fread` if it fits your case. It's all about control and what your program needs to chew on.

Behind the scenes, `fopen` tells your operating system, "Hey, I'll need this file, give me access!" And the system says okay by handing back a `FILE` pointer. The `fgetc` function whispers to the file pointer, "Give me the next byte, will you?" And it does, until it hits EOF, the End of File marker.

Alternatives? Sure. You've got ` fscanf` for formatted reads, `getline` for the modern guys, or low-level `read` system calls if you want to be close to the metal. And don't forget, after the last byte is read, be polite and `fclose` the file.

## See Also

To dive deeper, check out these:

- C Standard Library documentation: [https://en.cppreference.com/w/c/io](https://en.cppreference.com/w/c/io)
- GNU C Library Reference Manual: [https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html](https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html)
- Learn more about different reading functions: [https://www.tutorialspoint.com/c_standard_library/c_function_fread.htm](https://www.tutorialspoint.com/c_standard_library/c_function_fread.htm)
