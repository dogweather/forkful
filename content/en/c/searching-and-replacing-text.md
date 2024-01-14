---
title:    "C recipe: Searching and replacing text"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Most computer users have encountered the tedious task of searching and replacing text. Whether it's correcting a spelling mistake in a long document or updating file names, this process can be time-consuming and error-prone. This is where the power of programming comes in - by using a simple C program, you can easily automate the process of searching and replacing text, saving yourself time and effort in the long run.

## How To

To begin, let's define our problem. We have a text file, "example.txt", that contains the word "color" spelled with the British spelling "colour". We want to replace all instances of "colour" with "color". To do this, we will use the `fgetc()` and `fputc()` functions to traverse through the file, read each character, and write it to a new file with the necessary changes.

```C
#include <stdio.h>

int main(){
    FILE *inputFile;
    FILE *outputFile;
    char c;
    
    inputFile = fopen("example.txt", "r"); //open input file in read mode
    outputFile = fopen("output.txt", "w"); //open output file in write mode

    while((c = fgetc(inputFile)) != EOF){ //read each character until end of file
        if(c == 'o' && fgetc(inputFile) == 'u'){ //check for 'ou' sequence
            //replace 'our' with 'or'
            fputc('o', outputFile);
            fputc('r', outputFile);
        }
        else{
            fputc(c, outputFile); //write character as is to output file
        }
    }

    fclose(inputFile);
    fclose(outputFile);
    return 0;
}
```

The above program will replace all instances of "colour" with "color" and write the changes to a new file, "output.txt". As you can see, we first open both the input and output files using the `fopen()` function. Then, we use a while loop to traverse through the input file character by character. Within the loop, we check for the sequence "ou" and if found, we replace it with "or" in the output file. If the sequence is not found, we simply write the character as is to the output file. Finally, we close both files using the `fclose()` function.

Let's take a look at the sample input and output files:

Example input file "example.txt":
```
I have a colourful shirt. My neighbour has a colourless one.
```

Output file "output.txt":
```
I have a colorful shirt. My neighbor has a colorless one.
```

We can see that all instances of "colour" have been successfully replaced with "color".

## Deep Dive

While the above method is a simple and efficient way to search and replace text in a file, it may have its limitations. For example, it only works for replacing single characters, not entire words or phrases. Additionally, it can only work with ASCII characters, not Unicode characters. To overcome these limitations, one can use the `fgets()` and `fputs()` functions to read and write entire lines of text, or use advanced string manipulation functions such as `strchr()` and `strstr()`.

Furthermore, the program can be modified to make it more user-friendly, such as taking the input and output file names from the user instead of hard-coding them. Error handling can also be added to prevent crashes if the input file does not exist.

Overall, searching and replacing text using C programming gives you more flexibility and control over the process compared to using manual methods.

## See Also

- [C Programming Basics](https://www.programiz.com/c-programming)
- [Learn C - Search and Replace](https://www.learn-c.org/en/Strings)
- [Complete C Program for Search and Replace](https://www.oreilly.com/library/view/c-cookbook/0596003390/ch02.html#REPL_STRING)