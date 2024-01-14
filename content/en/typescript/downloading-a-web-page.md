---
title:                "TypeScript recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Why

Downloading a web page is a fundamental task for any web developer. It allows you to retrieve data and information from the internet, which can be used to create dynamic and interactive websites. In this blog post, we will discuss the process of downloading a web page using TypeScript, and why it is an essential skill for web development.

# How To

To download a web page using TypeScript, we will be using the `http` module from the Node.js runtime environment. This module provides functionality for making HTTP requests and handling responses. We will use the `get` method to make a GET request to the desired web page.

```TypeScript
import * as http from 'http';

// Define the URL of the web page to be downloaded
const url = 'https://www.example.com';

// Make a GET request to the URL
http.get(url, (response) => {
  // Handle the response data
  let data = '';

  // Concatenate the response data into a string
  response.on('data', (chunk) => {
    data += chunk;
  });

  // Handle the end of the response
  response.on('end', () => {
    // Print the downloaded web page
    console.log(data);
  });
})
```

Running this code will log the HTML content of the web page to the console. However, we can also use the response data for other purposes, such as parsing and extracting specific information from it. For example, we can use a library like Cheerio to parse the HTML and extract data from specific elements.

```TypeScript
import * as http from 'http';
import * as cheerio from 'cheerio';

// Define the URL of the web page to be downloaded
const url = 'https://www.example.com';

// Make a GET request to the URL
http.get(url, (response) => {
  // Handle the response data
  let data = '';

  // Concatenate the response data into a string
  response.on('data', (chunk) => {
    data += chunk;
  });

  // Handle the end of the response
  response.on('end', () => {
    // Use Cheerio to load the HTML
    const $ = cheerio.load(data);

    // Get the title of the web page
    const pageTitle = $('h1').text();

    // Print the title to the console
    console.log(pageTitle);
  });
})
```

This code will log the title of the web page to the console. With this approach, we can manipulate and use the downloaded web page data in various ways, depending on our specific needs and goals.

# Deep Dive

Downloading a web page in TypeScript is a straightforward process, thanks to the `http` module and other libraries like Cheerio. However, it is essential to note that there may be differences in the response data format, depending on the content-type header of the web page. For example, if the web page contains JSON data, we can use the `response.json()` method instead of the `response.on('data', ...)` method to handle the response data.

Furthermore, we can also set options and headers for our HTTP request using the `http.get` method. This allows us to customize our request and handle authentication and other types of requests.

# See Also

- Node.js `http` module documentation: https://nodejs.org/api/http.html
- Cheerio documentation: https://cheerio.js.org/