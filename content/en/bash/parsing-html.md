---
title:                "Bash recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Why

If you've ever worked with HTML, you know it can be a bit confusing to navigate through the different elements and tags. This is where parsing HTML comes in. By parsing HTML, you can easily extract specific data from a webpage and use it for various purposes. It can save you a lot of time and effort while working with HTML documents.

## How To

To parse HTML in Bash, we will be using the `grep` command. This command is useful for searching through text and finding specific patterns. Let's say we have the following HTML document:

```
<html>
  <head>
    <title>My Personal Website</title>
  </head>
  <body>
    <h1>Welcome to my blog!</h1>
    <p>This is a blog post about parsing HTML using Bash. I hope you find it helpful!</p>
  </body>
</html>
```

To extract the title of this webpage, we can use the following command:

```
title=$(grep -o '<title>.*</title>' index.html | cut -d ">" -f 2 | cut -d "<" -f 1)
```

Let's break down this command:

- `grep -o '<title>.*</title>' index.html`: This command searches for the `<title>` tag and prints the entire line where it is found.
- `cut -d ">" -f 2`: This command splits the line using ">" as the delimiter and prints the second field, which is the title we want.
- `cut -d "<" -f 1`: This command splits the title using "<" as the delimiter and prints the first field, which is the actual title.

So, our final output will be:

```
$title: My Personal Website
```

We can also use this method to extract other data from the HTML document, such as the header and paragraph content.

## Deep Dive

While the method we used above works for simple HTML documents, it may not work for more complex ones. For example, if the title tag is nested within other elements, the command will not be able to extract it successfully.

To handle such cases, we can use a tool called `sed` (stream editor). This tool allows us to search and replace text within a file. Let's say we have a more complex HTML document:

```
<html>
  <head>
    <meta>
      <title>My Personal Website</title>
    </meta>
  </head>
  <body>
    <div>
      <h1>Welcome to my blog!</h1>
    </div>
    <p>This is a blog post about parsing HTML using Bash. I hope you find it helpful!</p>
  </body>
</html>
```

To extract the title from this document, we can use the following command:

```
title=$(sed -n '/<title>/,/<\/title>/p' index.html | sed -e 's/<[^>]*>//g')
```

Let's break down this command:

- `sed -n '/<title>/,/<\/title>/p' index.html`: This command searches for the <title> tag and prints the lines between the opening and closing tags.
- `sed -e 's/<[^>]*>//g'`: This command removes all HTML tags from the extracted lines, leaving us with just the title.

So, our final output will be:

```
$title: My Personal Website
```

## See Also

- [Bash Commands](https://www.geeksforgeeks.org/bash-command-linux/)
- [grep command](https://www.geeksforgeeks.org/grep-command-in-linux-unix/)
- [sed command](https://www.geeksforgeeks.org/sed-command-in-linux-unix/)