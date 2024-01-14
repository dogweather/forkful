---
title:                "Gleam recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
In the world of writing code, there are endless possibilities and use cases for every programming language. One popular language that often flies under the radar is Gleam. This functional programming language has many features that make it unique, including the ability to easily download web pages. But why would someone want to do that?

If you are a developer or programmer, chances are you have come across situations where you need to retrieve information from a website. This could be for data analysis, web scraping, or simply accessing content from another site. Whatever the case may be, downloading a web page in Gleam can be a useful and efficient tool.

## How To
To download a web page in Gleam, we will be using the `httpc` module from the standard library. This module provides functions for making HTTP requests, which is exactly what we need for our task.

To get started, we first need to import the `httpc` module. This can be done by adding the following line at the top of our Gleam file:

```Gleam
import httpc
```

Next, we'll use the `get` function from `httpc` to make a GET request to the URL of the webpage we want to download. This function takes in a URL as a string and returns a `Result` type. We can then pattern match on this result to handle any errors that may occur.

```Gleam
let result = httpc.get("https://www.example.com")
case result {
    Ok(response) -> {
        // Do something with the response
        io.print(response.body)
    }
    Err(error) -> {
        // Handle the error
        log.error(error.message)
    }
}
```

In the above code, we are using the `io` module to print the body of the response to our console. However, you can use the response however you see fit, depending on your specific use case. You can also add headers and query parameters to your request by passing them as additional arguments to the `get` function.

## Deep Dive
Digging deeper into the `get` function, we can see that it takes in a second argument of type `Options`. This allows us to customize our request even further by specifying things like timeouts, SSL verification, and more.

For example, if we wanted to set a timeout of 10 seconds for our request, we can do so by creating an `Options` record and passing it to the `get` function.

```Gleam
let options = httpc.options(timeout: time.from_seconds(10))
let result = httpc.get("https://www.example.com", options)
```

The `Options` record has many more configurable options which you can explore in the Gleam documentation.

## See Also
For more information on downloading web pages in Gleam, check out the official documentation for the `httpc` module. You can also explore other useful modules in the Gleam standard library like `io` and `time` to enhance your web page downloading experience.

- https://gleam.run/modules/httpc.html
- https://gleam.run/modules/io.html
- https://gleam.run/modules/time.html