---
title:                "Читання текстового файлу"
date:                  2024-01-20T17:53:49.252123-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)

Читання текстового файла - це процес зчитування даних з файлу, який зберігає текст. Програмісти читають файли для обробки інформації, налаштувань, конфігурацій або даних, ввідних користувачем.

## How to: (Як це зробити:)

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *filePointer;
    char filename[] = "example.txt";
    char ch;

    filePointer = fopen(filename, "r");
    if (filePointer == NULL) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    while ((ch = fgetc(filePointer)) != EOF) {
        putchar(ch);
    }

    fclose(filePointer);
    return 0;
}
```
Output sample (Приклад виводу):
```
This is a line of text.
Here's another line.
```

## Deep Dive (Детальний огляд)

Reading text files in C has a long history - since the language's inception. `fopen`, `fread`, and `fgets` are standard functions to open files and read from them. `fgetc` reads a single character at a time. Alternatives include reading chunks with `fread` or line-by-line with `fgets`, depending on your needs.

Memory management is important. Always close files with `fclose` to avoid leaks. Large files require attention to buffer sizes and error handling to maintain performance and prevent crashes.

C stdio's buffered I/O can be bypassed with system-level functions like `read` found in `unistd.h` for POSIX systems. However, such direct methods often mean more complex error handling and are less portable.

## See Also (Дивіться також)

- C Standard Library documentation: https://en.cppreference.com/w/c/io
- C Programming by K&R (book).
- POSIX System Calls documentation: https://man7.org/linux/man-pages/man2/read.2.html
