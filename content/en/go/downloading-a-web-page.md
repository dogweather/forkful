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

## What & Why?

Downloading a web page refers to the process of retrieving the content and code of a webpage from a server and displaying it on a device. Programmers often do this in order to create web crawlers, scrapers, or simply to gather data from websites for analysis.

## How to:

```Go
package main

import (
	"fmt"
	"net/http"
)

func main() {
	// Specify the URL of the webpage you want to download
	url := "https://www.example.com"

	// Make a GET request to the URL
	response, err := http.Get(url)
	if err != nil {
		fmt.Println("Error fetching webpage:", err)
		return
	}

	// Read the response body
	defer response.Body.Close()
	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		fmt.Println("Error reading response body:", err)
		return
	}

	// Print the webpage content
	fmt.Println(string(body))
}
```

Output:
```
<!DOCTYPE html>
<html>
<head>
	<title>Example Domain</title>
	<meta charset="utf-8" />
	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<meta name="viewport" content="width=device-width, initial-scale=1" />
	<style type="text/css">
		body {
			background-color: #f0f0f2;
			margin: 0;
			padding: 0;
			font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
			
		}
		...
		...
		...
</html>
```

## Deep Dive:

Downloading web pages became popular in the late 1990s with the rise of the World Wide Web. It is commonly used for tasks such as web scraping, where data is collected from websites for various purposes such as market analysis, price comparisons, or content aggregation. Other alternatives to downloading web pages include using web APIs or using browser automation tools.

In Go, the `http` package provides the `Get()` function to make a GET request to a URL and retrieve the webpage content. By default, the `Get()` function follows any redirects and handles any network errors, making it a simple and efficient solution for downloading web pages.

## See Also:

- [Official Golang documentation for the `http` package](https://golang.org/pkg/net/http/)
- [Ben Johnson's blog post on web scraping in Go](https://www.usegolang.com/web-scraping-in-go/)
- [A comparison between web scraping and web APIs](https://rapidapi.com/blog/web-scraping-vs-web-apis/)