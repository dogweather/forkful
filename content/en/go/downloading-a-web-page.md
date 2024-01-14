---
title:                "Go recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

In today's digital age, information is just a click away. We constantly find ourselves in need of downloading web pages to access important documents, images, or videos for work, school, or personal use. This is where the power of Go programming comes in. With its simple and efficient syntax, Go makes it easy for us to download web pages and retrieve the information we need.

## How To

To start with, we need to import the `net/http` package to make HTTP requests. We will also use the `io/ioutil` package to read the response body. Let's take a look at the code below:

```Go
package main

import (
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    // Make GET request to desired URL
    response, err := http.Get("https://example.com")

    // Handle error
    if err != nil {
        fmt.Println("Error: ", err)
    }

    // Read response body
    body, err := ioutil.ReadAll(response.Body)

    // Handle error
    if err != nil {
        fmt.Println("Error: ", err)
    }

    // Convert response body to string and print
    fmt.Println(string(body))

    // Close response body
    response.Body.Close()
}
```

In the code above, we make a GET request to the URL `https://example.com` and read the response body. Then, we convert the body to a string and print it. Don't forget to close the response body at the end.

If we run this code, we should see the HTML of the webpage printed in our console. Easy, right?

## Deep Dive

Let's take a deeper dive into the code and understand what's happening.

Firstly, we use the `http.Get` method to make a GET request to the desired URL. This method returns a response object and an error. Next, we use the `ioutil` package to read the body of the response. We then convert the body to a string and print it. Finally, we close the response body. This whole process can also be done using the `http.Client` and `http.Request` structs. However, using the `Get` method is a more convenient and concise approach.

One thing to keep in mind is that while making HTTP requests, it is important to handle errors properly to avoid any unexpected behavior in our code. This is why we are using `if` statements to check for any errors and handle them accordingly.

## See Also

- [Go Net/HTTP Package](https://golang.org/pkg/net/http/)
- [Go IO/IOUtil Package](https://golang.org/pkg/io/ioutil/)
- [Go HTTP Client](https://blog.golang.org/go1.8-rc2)

Happy downloading!