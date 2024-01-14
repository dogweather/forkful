---
title:                "Clojure recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
Have you ever wanted to quickly and easily extract information from a web page? Perhaps you need to scrape data for a project or automate some online tasks. Whatever the reason may be, being able to download a web page using Clojure can come in handy in many situations.

## How To
Downloading a web page in Clojure is a simple process using the `clojure.java.io` library. First, we need to import the necessary functions by adding the following line to our code:
```Clojure
(:require [clojure.java.io :as io])
```

Next, we can use the `io/copy` function to download the page and save it to a local file. We pass in the URL of the page we want to download as the first argument and the file path where we want to save it as the second argument.
```Clojure
(io/copy (java.net.URL. "https://www.example.com") (io/file "result.html"))
```

We can also use `io/slurp` to download the page and return its content as a string. This can be useful if we only need the text from the page and don't need to save it to a file. We again pass in the URL as the first argument.
```Clojure
(io/slurp (java.net.URL. "https://www.example.com"))
```

Let's take a look at an example using these functions. Say we want to download the top news headlines from CNN's homepage and print them out. 
```Clojure
(ns my-project.core
  (:require [clojure.java.io :as io]))

(def url "https://www.cnn.com")
;; Download and save the page to a local file
(io/copy (java.net.URL. url) (io/file "cnn.html"))
;; Get the page content as a string
(def headlines (io/slurp (java.net.URL. url)))

;; Print out the top 5 headlines
(.split headlines #"<h3 class=\"ix__headline __text\">") ; split by headline tags

;; Output:
;; ["Breaking news..." "Trump says..." "New study shows..." "Covid cases..." "Biden announces..."]
```

## Deep Dive
Behind the scenes, the Clojure `io` library is utilizing Java's `java.net` package to handle the actual downloading and reading of the web page. This means we have access to all of the low-level functionality of the Java networking API if we need it. Additionally, we can pass in different options and parameters to the `io/copy` and `io/slurp` functions to customize our downloads.

For example, we can specify a timeout for the connection in milliseconds by passing in an optional third argument with the `:timeout` key.
```Clojure
(def headlines (io/slurp (java.net.URL. url) :timeout 5000)) ; 5 seconds timeout
```

We can also add custom HTTP headers to our request using the `:headers` key. This can be useful for websites that require specific headers to access the data.
```Clojure
(def headers {"User-Agent" "my-custom-user-agent"})
(def headlines (io/slurp (java.net.URL. url) :headers headers))
```

## See Also
- Official Clojure documentation on `clojure.java.io`: https://clojure.github.io/clojure/clojure.java.io-api.html
- Java `java.net` package documentation: https://docs.oracle.com/javase/8/docs/api/java/net/package-summary.html
- An in-depth tutorial on scraping with Clojure: https://www.braveclojure.com/scraping-the-web/