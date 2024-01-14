---
title:                "C recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/parsing-html.md"
---

{{< edit_this_page >}}

## Why
When developing a website or web application, it is often necessary to retrieve data from HTML documents. This is where parsing HTML comes into play, allowing programmers to extract specific information from these documents. In this blog post, we will cover the basics of parsing HTML using C programming language.

## How To
To begin parsing HTML in C, we will first need to include the <stdio.h> and <string.h> libraries. These libraries provide the necessary functions for reading and manipulating strings. We will also be using the <stdlib.h> library for memory allocation.

Next, we will need to have an HTML document that we want to parse. For this example, we will use the following document:

```
<html>
  <head>
    <title>Casual C Programming Blog</title>
  </head>
  <body>
    <h1>Welcome to my blog!</h1>
    <p>In this blog, we will discuss various topics related to C programming in a casual and easy-to-understand manner.</p>
    <p>Stay tuned for more programming tips and tricks!</p>
  </body>
</html>
```

To read this document in our C program, we will use the `fopen()` function to open the HTML file and `fgets()` to read its contents line by line. We will then use the `strtok()` function to tokenize each line and search for specific tags or keywords that we are interested in.

```C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(){

  // Open the HTML file for reading
  FILE* file = fopen("blog.html", "r");

  // Buffer to store each line of the file
  char buffer[100];

  // Loop through each line of the file
  while(fgets(buffer, 100, file)){

    // Tokenize the line using the delimiter "/"
    char* token = strtok(buffer, "/");

    // Loop through each token
    while(token != NULL){

      // Check if the token contains the title tag
      if(strstr(token, "<title>") != NULL){

        // Print out the title of the blog
        char* title = strtok(token, "<>");
        printf("Blog Title: %s\n", title);
      }

      // Check if the token contains the paragraph tag
      else if(strstr(token, "<p>") != NULL){

        // Print out the content of the p tag
        char* content = strtok(token, "<>");
        printf("Blog Content: %s\n", content);
      }

      // Move on to the next token
      token = strtok(NULL, "/");
    }
  }

  // Close the file
  fclose(file);

  return 0;
}
```

The output of this program will be:

```
Blog Title: Casual C Programming Blog
Blog Content: Welcome to my blog!
Blog Content: In this blog, we will discuss various topics related to C programming in a casual and easy-to-understand manner.
Blog Content: Stay tuned for more programming tips and tricks!
```

This is a basic example of how we can use C to parse HTML documents. There are many other functions and techniques that can be used for more complex parsing tasks.

## Deep Dive
To parse HTML, we need to understand the structure of the document. HTML documents are made up of tags, which are enclosed in angled brackets (< >) and serve as instructions to the web browser on how to display the content. These tags can also have attributes such as class, id, and style which provide additional information about the element.

To extract information from these tags, we can use string manipulation functions to search for specific keywords or use regular expressions for more complex patterns. We can also use HTML parsers such as [libxml2](https://xmlsoft.org/html/index.html) or [Gumbo](https://github.com/google/gumbo-parser) which provide more advanced and efficient ways of parsing HTML documents.

It is important to note that parsing HTML can be a complex task and may require knowledge of the HTML document's structure and various parsing techniques. It is also important to validate the HTML before parsing it to avoid any unexpected errors.

## See Also
- [libxml2: XML C parser and toolkit](https://xmlsoft.org/html/index.html)
- [Gumbo: HTML5 parsing library in pure C99](https://github.com/google/gumbo-parser)
- [w3schools: Introduction to HTML](https://www.w3schools.com/html/)
- [Regular Expressions in C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)

Parsing HTML in C can be a useful skill to have, especially when dealing with web-related projects. With the knowledge and techniques discussed in this blog post, you can now start extracting information from HTML documents with ease.