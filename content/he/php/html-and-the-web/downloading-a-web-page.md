---
title:                "הורדת דף אינטרנט"
aliases:
- /he/php/downloading-a-web-page/
date:                  2024-01-20T17:44:26.280428-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
Downloading a web page means grabbing the HTML content from a URL. Programmers do this to process, analyze or display the content in a different context.

## איך לעשות:
```php
<?php
$url = 'http://example.com';
$content = file_get_contents($url);
if ($content !== false) {
    // Do something with the content
    echo $content;
} else {
    // Handle the error
    echo "Couldn't download the page.";
}
?>
```

Sample Output:
```html
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</head>
<body>
...
</body>
</html>
```

## עיון מעמיק:
Back in the day, we had to use CURL for downloading web pages, which gave us more control but was verbose. Now, `file_get_contents` is a handy alternative for simple tasks. But remember, for complex tasks or error handling, CURL is still preferable. When using `file_get_contents()`, check for its return value, false, which indicates failure. To handle HTTP headers, timeouts, and other finer details, delve into `stream_context_create()`.

## ראה גם:
- PHP Manual on `file_get_contents`: https://www.php.net/manual/en/function.file-get-contents.php
- PHP Manual on `stream_context_create()`: https://www.php.net/manual/en/function.stream-context-create.php
- PHP CURL Documentation for advanced usage: https://www.php.net/manual/en/book.curl.php
