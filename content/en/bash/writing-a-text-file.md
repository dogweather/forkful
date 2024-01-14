---
title:                "Bash recipe: Writing a text file"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why 
Text files are an essential part of Bash programming, allowing you to store and manipulate data in a simple and organized way. By learning how to write a text file, you can improve your scripting skills and make your code more efficient and versatile.

## How To

To create a text file in Bash, use the `touch` command followed by the name of the file you want to create. For example:

```Bash
touch my_file.txt
```

This will create an empty text file called `my_file.txt` in the current directory. 

To add content to your file, you can use the `echo` command, which prints text to the terminal. For example:

```Bash
echo "Hello, World!" >> my_file.txt
```

This will append the text "Hello, World!" to the end of `my_file.txt`. 

You can also use `cat` to display the contents of a file, like this:

```Bash
cat my_file.txt
```

This will print the contents of `my_file.txt`, which in this case would be "Hello, World!"

## Deep Dive

When writing a text file in Bash, it's important to understand the different types of redirection. Redirection allows you to control where the output of a command is sent. In the previous example, we used the `>>` redirect symbol to append the output of `echo` to our file. 

Another important redirect symbol is `>`, which will overwrite any existing content in the file instead of appending it. For example, if we use `>` instead of `>>` in our `echo` command, the text "Hello, World!" would replace any existing content in `my_file.txt` instead of being appended to it.

You can also use the `<<` redirect symbol to redirect the output of a command to a **here document**. A here document is a special type of text block that can be redirected to a file or to standard input. For example:

```Bash
cat << EOF > my_new_file.txt
This is the first line of my new file.
This is the second line.
EOF
```

This will create a new file called `my_new_file.txt` and add the text "This is the first line of my new file." followed by "This is the second line." on the next line. 

## See Also

- The Bash Beginners Guide: https://tldp.org/LDP/Bash-Beginners-Guide/html/
- Redirection in Bash: https://www.howtogeek.com/102990/a-beginners-guide-to-using-redirecting-input-and-output-in-linux/ 
- More about Here Documents: https://linuxhint.com/linux-here-document-tutorial/