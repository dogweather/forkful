---
date: 2024-01-20 17:44:08.930540-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) Let's use `curl` in Fish to download a webpage."
lastmod: '2024-03-13T22:44:50.066019-06:00'
model: gpt-4-1106-preview
summary: Let's use `curl` in Fish to download a webpage.
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

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
