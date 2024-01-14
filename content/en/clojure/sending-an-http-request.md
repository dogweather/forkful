---
title:                "Clojure recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is a crucial aspect of web development and automation. It allows communication between servers and clients, making it possible to retrieve and send data, perform actions, and integrate multiple systems together. Knowing how to send HTTP requests in Clojure can greatly enhance your ability to create dynamic and interactive web applications.

## How To

Sending HTTP requests in Clojure is made easy with the help of the `clj-http` library, which provides a user-friendly API for making HTTP requests. First, we need to install the library by adding it to our `project.clj` file:

```Clojure
[clj-http "3.11.0"]
```

Once the library is installed, we can import it into our namespace:

```Clojure
(:require [clj-http.client :as http])
```

Now, we can make an HTTP GET request to a specific URL and retrieve its response:

```Clojure
(def response (http/get "https://www.google.com"))
```

To get the response body, we can use the `:body` key:

```Clojure
(:body response) ; returns the HTML of the Google homepage
```

We can also include query parameters in our request by passing a map to the `:params` key:

```Clojure
(def response (http/get "https://www.example.com" {:params {:id 1234 :name "John"}}))
```

To send a POST request, we can use the `http/post` function and pass in the URL, headers, and body:

```Clojure
(def response (http/post "https://www.example.com" {:headers {"Content-Type" "application/json"} :body "{\"name\": \"John\"}"}))
```

The response returned by these functions is a map containing the status, headers, and body of the HTTP request. We can access the status code using the `:status` key:

```Clojure
(:status response) ; returns the status code of the request
```

## Deep Dive

Behind the scenes, the `clj-http` library uses the Apache HTTP Client to make HTTP requests. This library handles all the low-level details of opening and managing connections, sending and receiving data, and handling errors. This allows us to focus on the logic of our requests without worrying about the underlying implementation.

In addition to the basic GET and POST requests, the `clj-http` library also supports other HTTP methods such as PUT, PATCH, DELETE, and OPTIONS. It also allows for the customization of headers, cookies, and other request parameters. By familiarizing yourself with the library's API, you can unleash the full potential of sending HTTP requests in Clojure.

## See Also

- [clj-http documentation](https://github.com/dakrone/clj-http)
- [Official Apache HTTP Client website](https://hc.apache.org/httpcomponents-client-4.5.x/index.html)
- [HTTP Methods in Clojure](https://technpol.wordpress.com/2017/02/02/http-methods-in-clojure/)