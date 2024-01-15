---
title:                "Parsing html"
html_title:           "Bash recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Parsing HTML is a useful skill for anyone who works with web development or data extraction. It allows you to easily extract specific data from HTML documents and manipulate it for further use.

## How To

To parse HTML using Bash, you can use the powerful command line tool `sed`. This tool allows you to perform string replacements and manipulations on text, making it perfect for parsing HTML. 

First, we need to get the HTML document we want to parse. We can use `curl` to retrieve the HTML and save it to a variable:

```Bash
html=$(curl www.example.com)
```

Next, we can use `sed` to extract the data we want from the HTML. For example, if we want to extract all the links from the document, we can use the following command:

```Bash
links=$(echo "$html" | sed -n 's/<a[^>]*href="\([^"]*\).*/\1/p')
```

This command will search for all lines containing `<a href="...">` and extract the link from within the quotes. The result will be stored in the `links` variable.

To further manipulate the data, we can use other commands such as `grep`, `awk`, or `cut` in combination with `sed`. For example, we can use `grep` to filter out specific links based on a pattern:

```Bash
filtered_links=$(echo "$links" | grep "example")
```

Finally, we can use `echo` to print out the final result:

```Bash
echo "$filtered_links"
```

Which will output something like:

```Bash
http://www.example.com
https://www.example.com/contact
```

## Deep Dive

Parsing HTML with `sed` may seem intimidating at first, but with some practice, it can become a powerful tool in your web development arsenal. Using `sed`'s regular expressions, you can perform complex string manipulations and filter out specific patterns within the HTML.

One important thing to note is that HTML is not a regular language, which means it is not perfectly suited for parsing with regular expressions. This can lead to errors or incorrect matches in some cases. Therefore, it is important to carefully test and refine your `sed` commands to ensure they are accurately parsing the desired data.

For more complex projects, you may want to consider using a specialized HTML parsing library such as `html-xml-utils` or `libxml2` which provide more robust and accurate methods for parsing HTML.

## See Also
- [GNU Sed User's Manual](https://www.gnu.org/software/sed/manual/sed.html)
- [Bash Programming - Introduction](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
- [html-xml-utils](https://www.w3.org/Tools/html-xml-utils/)
- [libxml2](http://xmlsoft.org/libxml2/)