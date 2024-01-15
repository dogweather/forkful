---
title:                "Downloading a web page"
html_title:           "Gleam recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Why
So you want to download a web page using the Gleam programming language? Maybe you want to perform some data scraping or automate a task. Whatever your reason may be, Gleam's web page downloading capabilities make it a breeze to extract data from any website.

##How To
To download a web page using Gleam, simply use the `http` library and the `get_text` function. Let's take a look at an example:

```
Gleam module DownloadWebPage

import http

pub fn main() {
  let response = http.get_text("https://www.example.com").ok()
  match response {
    Ok(result) -> {
      // Process the downloaded web page here
    }
    Err(_) -> {
      // Handle any errors here
    }
  }
}
```

In the above code, we are using the `get_text` function to download the web page from the URL provided. The `.ok()` method converts the response into a `Result` type, which we can then match against to check for any errors.

##Deep Dive
Now, let's take a deeper look at the `get_text` function. This function takes in a `String` of the URL and returns a `Result` with the downloaded page as a `String`. This makes it easy to extract data from the web page using string manipulation methods.

If you need more control over the web page downloading process, you can also use the `get` function from the `http` library. This function allows you to customize the request by adding headers or setting a timeout.

##See Also
To learn more about the `http` library and its functions, check out the Gleam documentation [here](https://gleam.run/documentation/stdlib/http). You can also explore other useful libraries for web scraping and automation, such as `html` and `jsoup`, to further enhance your web page downloading capabilities. Happy coding!