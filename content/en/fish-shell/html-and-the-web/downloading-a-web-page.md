---
date: 2024-01-20 17:44:11.399176-07:00
description: 'How to: Here''s the quick and dirty way to download a web page using
  Fish Shell with the `curl` command.'
lastmod: '2024-03-13T22:45:00.473188-06:00'
model: gpt-4-1106-preview
summary: Here's the quick and dirty way to download a web page using Fish Shell with
  the `curl` command.
title: Downloading a web page
weight: 42
---

## How to:
Here's the quick and dirty way to download a web page using Fish Shell with the `curl` command:

```fish
curl -O http://example.com/
```

This command fetches the contents of the webpage and saves it with the same name as the filename on the server (`index.html` for most cases).

Now, say you want to save it with a different name:

```fish
curl -o my_page.html http://example.com/
```

Want to see what you're fetching? Here's how to print it to the console:

```fish
curl http://example.com/
```

Sample output might look like this:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive
Back in the early days, fetching web pages was more command-line magic than anything else. Tools like `wget` and `curl` became staples. `curl`, around since '97, has stood the test of time for delivering data using URL syntax.

Why `curl` over `wget`? `curl` is more of a data transfer swiss army knife, dealing with a range of protocols and data formats. While both can download web pages, `curl` can also upload data, and it supports more protocols and is often used as a back-end tool by other software.

Fish Shell itself doesn't download web pages; it's just the interface. But pair it with `curl`, and you've got a powerful yet simple one-liner web-fetching rig.

Some folks might bring up using more modern tools like `httpie` or browser-based automation with tools like Selenium for more complex tasks like dealing with Javascript-heavy pages. However, for the quick and straightforward download, `curl` still holds the fort.

## See Also
- curl project website for more details: [https://curl.se/](https://curl.se/)
- For a deeper dive into HTTP operations with `curl`, see the man page: `man curl`
- httpie as a user-friendly HTTP client alternative: [https://httpie.org/](https://httpie.org/)
- Fish Shell documentation for handling other shell-related tasks: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
