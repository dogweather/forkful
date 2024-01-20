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

## What & Why?

Downloading a web page is just grabbing its data onto your local machine. It's an important tool for programmers since it enables data mining, offline browsing, and website testing.

## How to:

Straight to the show; let's download a webpage using Curl command in Bash:

```Bash
curl https://example.com > example.html
```

That's it. The webpage is down in 'example.html'. Now, to print it in the console:

```Bash
curl https://example.com
```

It's like Bash is reading the webpage for you.

## Deep Dive

Historically, this capability traces back to early internet days—when dial-up connections were still a thing. Now it's faster and more efficient.

You might be thinking, 'What if I don't want to use Curl command?' Well, as a programmer, it's healthy to have choices, and you have `wget`. Here's how it's done:

```Bash
wget https://example.com
```

Unlike Curl, `wget` automatically stores the page in a file.

Speaking of implementation details, both Curl and `wget` go through similar processes - a sequence of DNS resolution, TCP handshake, HTTP request, and then the actual data transfer.

## See Also

1. Curl vs. Wget: [https://daniel.haxx.se/docs/curl-vs-wget.html](https://daniel.haxx.se/docs/curl-vs-wget.html)
2. Curl man page: [https://curl.haxx.se/docs/manpage.html](https://curl.haxx.se/docs/manpage.html)
3. Wget man page: [https://www.gnu.org/software/wget/manual/wget.html](https://www.gnu.org/software/wget/manual/wget.html)