---
title:                "Downloading a web page"
html_title:           "Go recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to download a web page for a project or simply to read it offline? Instead of spending time copying and pasting the content, why not use a programming language to do the work for you? In this article, we will explore how to download a web page using Go, the current version of the popular programming language.

## How To

To get started, make sure you have Go installed on your computer. Then, create a new Go file and add the following import statement:

```
import "net/http"
```

Next, we will use the `Get()` function from the `http` package to make a GET request to the web page we want to download. Let's say we want to download the GitHub homepage, we would use the following code:

```
resp, err := http.Get("https://github.com/")
```

The `Get()` function takes in a string representing the URL of the web page we want to download and returns a response and an error. We assign these values to `resp` and `err` respectively.

To check if there was an error during the request, we can use a simple `if` statement:

```
if err != nil {
    // handle error
}
```

If there is no error, we can proceed to read the response body using the `ReadAll()` function from the `ioutil` package:

```
body, err := ioutil.ReadAll(resp.Body)
```

Again, we assign the response to `body` and check for any errors. Finally, we can print the downloaded web page by using `fmt.Println()` and passing in the `body` variable:

```
fmt.Println(string(body))
```

The `string()` function converts the body byte array to a string for easier printing. Your complete code should look like this:

```
import (
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    resp, err := http.Get("https://github.com/")

    if err != nil {
        // handle error
    }

    body, err := ioutil.ReadAll(resp.Body)

    if err != nil {
        // handle error
    }

    fmt.Println(string(body))
}
```

And the output should be the HTML code of the GitHub homepage. Congratulations, you have successfully downloaded a web page using Go!

## Deep Dive

Behind the scenes, the `Get()` function is actually creating an HTTP request, sending it to the server, and receiving a response. This is made possible by the `http` package, which provides functions for making HTTP requests and handling responses.

In our code, we used the `ReadAll()` function to read the response body. This function takes in a `Reader` object, which can be created by accessing the `Body` field of the response. The `Body` field is of type `io.ReadCloser`, which implements the `Reader` interface.

To learn more about the `http` package, check out the official documentation: https://golang.org/pkg/net/http/

## See Also

- https://golang.org/doc/
- https://github.com/golang/go/wiki/Learn