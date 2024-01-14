---
title:                "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
Downloading a web page can be a useful skill to have for various reasons. It allows you to save a webpage for offline viewing, access restricted content, or simply save a backup of a favorite website.

## How To
To download a web page using Bash, we can use the `wget` command. This command retrieves files from a given URL and saves them to our current directory. Here's an example:

```Bash
wget www.example.com
```

This will download the index.html file from the given URL and save it as `index.html` in our current directory. We can also specify a different file name or path as needed.

To download a webpage with all its assets, such as images and CSS files, we can use the `-p` flag:

```Bash
wget -p www.example.com
```

This will create a folder named `www.example.com` and save all the webpage's assets in it. 

## Deep Dive
When downloading a webpage, `wget` also supports various options to customize the download. One useful option is the `-m` flag which will download the complete webpage with all its nested resources, such as linked pages, images, and CSS files.

```Bash
wget -m www.example.com
```

Additionally, we can also specify a user-agent to mimic a specific browser when downloading a webpage. This can be done by using the `--user-agent` flag followed by the user-agent string.

```Bash
wget --user-agent="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.0" www.example.com
```

This will make the server think we are accessing the webpage from a Firefox browser on a Windows operating system.

## See Also
- [Official `wget` Documentation](https://www.gnu.org/software/wget/)
- [The Ultimate `wget` Guide](https://www.lifewire.com/uses-of-wget-2201085)
- [Bash Scripting Tutorial for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)

Happy downloading!