---
title:                "Downloading a web page"
html_title:           "Fish Shell recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why 

Have you ever wanted to save a webpage for later offline reading, or to access it from a remote location with limited internet access? With Fish Shell, a command-line shell tool, you can effortlessly download web pages directly from the terminal. No need for browser extensions or separate downloading software. 

## How To

Using Fish Shell, the *curl* command is your go-to tool for downloading web pages. From a terminal window, simply type:
```Fish Shell
curl <URL> -o <filename>.html
```
This command will download the webpage at the specified URL and save it as an HTML file with the given filename.

You can also use the *wget* command, like this:
```Fish Shell
wget -O <filename>.html <URL>
```
This command works similarly to the *curl* command, allowing you to specify the output filename.

Another useful option is to use the *-L* flag, which follows any redirects and ensures that the final webpage is downloaded.
```Fish Shell
curl -L <URL> -o <filename>.html
```

## Deep Dive

In addition to saving web pages, Fish Shell also allows you to download specific files from a webpage. For example, you can use the *-O* flag to specify the output filename for a specific file:
```Fish Shell
curl -O <URL>/images/logo.png
```
This command will download the logo.png file from the specified URL and save it with its original filename.

You can also use the *-I* flag to download only the headers of a webpage, without downloading the actual content. This can be useful for checking URLs and file sizes before downloading:
```Fish Shell
curl -I <URL>
```

Furthermore, Fish Shell allows for basic authentication through the *-u* flag. This allows you to download web pages from sites that require a username and password for access:
```Fish Shell
curl -u <username>:<password> <URL> -o <filename>.html
```

## See Also

- [Fish Shell documentation](https://fishshell.com/docs/current/tutorial.html)
- [Curl documentation](https://curl.haxx.se/docs/manpage.html)
- [Wget manual](https://www.gnu.org/software/wget/manual/wget.html)