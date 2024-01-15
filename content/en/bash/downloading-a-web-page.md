---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

Have you ever come across a website with important information that you wanted to access offline? Or maybe you wanted to save a copy of an online recipe for later use? By downloading a web page, you can easily access its content without an internet connection.

## How To

To download a web page using Bash, you can use the "wget" command followed by the URL of the page you want to download. For example:

```Bash
wget https://www.example.com
```

This will download the homepage of the website and save it in the current directory. You can also specify a different directory to save the downloaded page by adding the "-P" flag followed by the directory path.

```Bash
wget -P /home/user/downloads https://www.example.com
```

You can also use the "curl" command to download a web page. Similar to "wget," you just need to specify the URL of the page.

```Bash
curl https://www.example.com
```

Both "wget" and "curl" will download the HTML source code of the webpage. If you want to save a specific file from the webpage, you can use the "-O" flag followed by the file name.

```Bash
wget https://www.example.com/myfile.pdf -O myfile.pdf
```

To download multiple files from a webpage, you can use the "-i" flag followed by a file containing the list of URLs. For example, create a text file named "urls.txt" with the URLs of the files you want to download, each on a separate line, and use the following command:

```Bash
wget -i urls.txt
```

## Deep Dive

Both "wget" and "curl" are command-line tools that are used for downloading files from URLs. They have various options and flags that you can use to customize the download process, such as setting download speed limits, including or excluding certain file types, and using authentication.

Additionally, you can also use these tools to perform various other operations apart from downloading, such as uploading files, testing API endpoints, and fetching server information.

## See Also

Check out these links for more information on downloading web pages using Bash:

- Bash scripting tutorial: https://ryanstutorials.net/bash-scripting-tutorial/
- "wget" manual: https://www.gnu.org/software/wget/
- "curl" manual: https://curl.haxx.se/docs/manpage.html