---
title:                "Завантаження веб-сторінки"
aliases:
- uk/fish-shell/downloading-a-web-page.md
date:                  2024-01-20T17:44:08.930540-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Downloading a web page means grabbing its HTML content from the internet. Programmers do this to automate data collection, test websites, or analyze online content.

## How to: (Як це зробити:)
Let's use `curl` in Fish to download a webpage:

```fish
curl -o mypage.html https://example.com
```
Sample output:

```plaintext
% Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                Dload  Upload   Total   Spent    Left  Speed
100  1256  100  1256    0     0   6358      0 --:--:-- --:--:-- --:--:--  6358
```
Now `mypage.html` holds the HTML content of the example website.

## Deep Dive (Детальніше)
In the early days of the internet, downloading web pages was mostly manual. Tools like `wget` and `curl` came along to streamline the process. `curl` is versatile, supporting different protocols and methods. While `curl` is the go-to in scripting, GUI-based apps like `Postman` also let you download web content, giving users interactive control.

If you want to do more than just download, such as parsing HTML, you might combine `curl` with other tools. Fish makes it easy to pipe `curl` output to processors like `jq` or `grep`.

Example with processing:
```fish
curl -s https://example.com | grep 'some-pattern'
```

## See Also (Додатково)
- Fish documentation: [fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- `curl` tutorial: [curl.haxx.se/docs/manual.html](https://curl.haxx.se/docs/manual.html)
- For parsing HTML, check out `pup`: [github.com/ericchiang/pup](https://github.com/ericchiang/pup)
- `wget` documentation for alternative downloading: [www.gnu.org/software/wget/manual/wget.html](https://www.gnu.org/software/wget/manual/wget.html)
