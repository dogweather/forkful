---
title:                "PHP recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
Have you ever found yourself wanting to save a webpage for offline viewing? Maybe you're going on a trip with limited internet access or want to keep a backup of important information. Whatever the reason, downloading a webpage can come in handy in many situations.

## How To
Downloading a webpage using PHP is a simple process that can be accomplished with just a few lines of code. First, we need to create a new instance of the `DomDocument` class, which will allow us to manipulate HTML documents. Then, we use the `loadHTMLFile()` method to load the webpage we want to download.

```
<?php
$doc = new DomDocument();
$doc->loadHTMLFile("https://www.example.com");
```

Next, we need to specify the file path and name where we want to save the webpage. We can do this by using the `saveHTMLFile()` method and passing in the desired file path and name.

```
<?php
$doc->saveHTMLFile("saved_page.html");
```

That's it! Our webpage has been downloaded and saved as an HTML file. We can now open it in any browser and view it offline.

## Deep Dive
While the above example is a simple demonstration of how to download a webpage using PHP, there are other methods and techniques that can be used. For example, you can also use the `file_get_contents()` function to retrieve the page source and then save it as an HTML file.

One thing to keep in mind when downloading a webpage is that the page's CSS and images may not be included in the saved file. This can result in an incomplete or distorted version of the webpage. To ensure that all elements are saved, you can use a library like cURL to retrieve and save all the page's resources.

## See Also
- [PHP DomDocument documentation](https://www.php.net/manual/en/class.domdocument.php)
- [PHP file_get_contents() documentation](https://www.php.net/manual/en/function.file-get-contents.php)
- [cURL library documentation](https://www.php.net/manual/en/book.curl.php)