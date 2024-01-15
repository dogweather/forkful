---
title:                "「ウェブページのダウンロード」"
html_title:           "Javascript: 「ウェブページのダウンロード」"
simple_title:         "「ウェブページのダウンロード」"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

Web pages are essential for accessing and sharing information, so having the ability to download them is crucial. This can be useful for offline viewing, saving important data, or creating backups.

## How To

```Javascript
const request = require('request');

// download a web page and save it as a text file
request('https://www.example.com', function(error, response, body) {
    if(!error && response.statusCode == 200) {
        fs.writeFile('page.txt', body, (err) => {
            if (err) throw err;
            console.log('Page downloaded and saved as page.txt');
        })
    }
});
```

Running this code will use the 'request' library to download the web page at the specified URL and save it as a text file named 'page.txt' in the same directory as your Javascript file. This can also be done with other programming languages and libraries, but Javascript provides a simple and versatile option.

## Deep Dive

The `request` library used in the code example is just one of many ways to download web pages in Javascript. Other popular libraries include `axios` and `node-fetch`. These libraries provide a convenient way to make network requests and handle the response data.

In addition to downloading web pages, Javascript can also be used to parse and extract information from them. This can be done using libraries like `cheerio` or the built-in `DOMParser`. These tools allow you to access specific elements on the page and extract data from them, making it easier to manipulate and analyze web page content.

See Also:

- [Request library](https://github.com/request/request)
- [Axios library](https://github.com/axios/axios)
- [Node-fetch library](https://github.com/node-fetch/node-fetch)
- [Cheerio library](https://github.com/cheeriojs/cheerio)
- [DOMParser documentation](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)