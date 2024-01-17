---
title:                "Downloading a web page"
html_title:           "Javascript recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page is the process of retrieving information from a website and saving it on your computer. Programmers often do this for tasks such as web scraping, data analysis, or building applications that require data from the web.

## How to:

To download a web page in Javascript, we can use the built-in `fetch` function. This function takes in the URL of the webpage we want to download and returns a promise that we can use to access the webpage's data. Here is an example of using `fetch` in action:

```Javascript
fetch("https://www.example.com")
  .then(response => {
    // do something with the response data
  })
  .catch(error => {
    // handle any error that may occur
  });
```

We can also use `async/await` syntax to make our code more readable. Here's an example:

```Javascript
async function fetchWebpage() {
  try {
    const response = await fetch("https://www.example.com");
    // do something with the response data
  } catch (error) {
    // handle any error that may occur
  }
}

fetchWebpage();
```

## Deep Dive:

Webpage downloading has been around since the early days of the internet when people used tools like wget and curl. With the rise of languages like Javascript, it has become easier to do with built-in functions like `fetch`. An alternative to using `fetch` is using libraries such as Axios or jQuery's `$.ajax` function which provide more options and customization.

When using `fetch`, we have access to the Response object, which contains information such as the status code and headers. We can also specify the HTTP request method and add any necessary headers. 

## See Also:

To learn more about the `fetch` function in Javascript, check out the following resources:

- [MDN Web Docs on Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Axios library](https://github.com/axios/axios)
- [jQuery.ajax()](https://api.jquery.com/jquery.ajax/) function.