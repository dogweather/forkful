---
title:                "Downloading a web page"
html_title:           "TypeScript recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
Web scraping, or the act of downloading information from a web page, has become more and more prominent in today's digital age. Whether it's for data analysis, market research, or automating repetitive tasks, downloading a web page in TypeScript can provide valuable information and save time.

## How To
Downloading a web page in TypeScript is a relatively simple process. First, we need to import the `https` module from the Node.js standard library. Then, we can use the `get` method to send a request to the desired URL and receive a response. Let's take a look at an example:

```TypeScript
import * as https from 'https';

https.get("https://www.example.com", (response) => {
  console.log(`Status Code: ${response.statusCode}`);
  
  // Print the HTML body of the web page
  response.on('data', (data) => {
    console.log(data.toString());
  });
  
  // Handle any errors
  response.on('error', (error) => {
    console.error(error);
  });
});
```

In this example, we are using the `get` method to send a GET request to the `example.com` web page. Once we receive a response, we can access the response object's `statusCode` property to check the status of the request. Then, we use the `response.on` method to handle the data and any errors that may occur. Finally, we convert the data to a string and print it to the console.

## Deep Dive
The `https` module provides various methods for making different kinds of requests, such as GET, POST, PUT, and DELETE. We can also pass additional options, headers, and data in our requests. Additionally, we can use third-party libraries like `cheerio` or `puppeteer` to parse and manipulate the HTML data we download.

It's important to note that web scraping may not always be allowed by website owners, so it's essential to follow best practices and adhere to their terms of use. It's also crucial to handle errors and to be mindful of any potential legal implications.

## See Also
- [Node.js `https` module documentation](https://nodejs.org/api/https.html)
- [Cheerio documentation](https://cheerio.js.org/)
- [Puppeteer documentation](https://pptr.dev/)