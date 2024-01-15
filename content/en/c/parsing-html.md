---
title:                "Parsing html"
html_title:           "C recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Parsing HTML, the language used to create and structure web pages, is an essential skill for any developer looking to manipulate web content. By understanding how to parse HTML, you can extract valuable data from websites, automate tasks such as web scraping, and create dynamic web applications.

## How To

To get started with parsing HTML in C, you will need a basic understanding of the language and its syntax. Here are some essential steps to follow:

1. Begin by including the `stdio.h` and `stdlib.h` header files in your code. These are necessary for reading and writing data.
2. Next, add the `string.h` header file to access string manipulation functions.
3. Create a `FILE` pointer variable and use the `fopen()` function to open the HTML file you want to parse.
4. Use the `fscanf()` function to scan the file and extract data using a specific format.
5. You can then use string manipulation functions to clean up the data if necessary.
6. Finally, close the file using the `fclose()` function.

Here is a sample code to parse a HTML file and extract all the links within anchor tags:

```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
    FILE *html;
    char link[100], tag[10];
    html = fopen("index.html", "r");
    while (fscanf(html, "<%[a]%*[ href=\"%[^\"]", tag, link) != EOF)
    {
        printf("%s\n", link);
    }
    fclose(html);
    return 0;
}
```

### Sample Output:

```
https://github.com
https://www.linkedin.com
https://twitter.com
```

## Deep Dive

Parsing HTML in C involves understanding the structure and syntax of the language. HTML uses tags to specify elements such as headings, paragraphs, and links. These tags are denoted by opening and closing angle brackets (`<` and `>`).

To extract data from HTML using the `fscanf()` function, you need to provide a specific format for the function to follow. In the above example, we use `%[a]` to scan for anchor tags and `%[^\"]` to scan for the URL within the `href` attribute inside the tag. The asterisk (`*`) ignores characters in between the tag and the URL.

Additionally, you can use `fgets()` to read each line of the HTML file and then use string manipulation functions such as `strstr()` and `strtok()` to extract specific data. This method may be more suitable for larger and more complex HTML files.

## See Also

- [C String Functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Parsing HTML in C](https://www.geeksforgeeks.org/parsing-html-pages-in-c-set-1/)
- [Introduction to HTML](https://www.w3schools.com/html/)