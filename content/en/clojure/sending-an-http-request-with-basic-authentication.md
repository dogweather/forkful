---
title:                "Clojure recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
Sending HTTP requests with basic authentication is essential for accessing secure resources on the web. It allows users to provide a username and password for verification before accessing sensitive information.

## How To

To send an HTTP request with basic authentication in Clojure, we will use the "clj-http" library. First, we need to include it in our project by adding the dependency in our "project.clj" file:

```Clojure
:dependencies [[clj-http "3.10.0"]]
```

Next, we need to require the library in our namespace:

```Clojure
(ns my-project.core
(:require [clj-http.client :as http]))
```

Now, we can use the "basic-auth" function provided by the library to send our request. We need to provide the URL, username, and password as parameters:

```Clojure
(http/get "https://example.com/secure/resource"
 :basic-auth {:username "myusername" :password "mypassword"})
```

This will result in a response that we can then use in our code. For example, we can print the status code and response body:

```Clojure
(let [response (http/get "https://example.com/secure/resource"
                       :basic-auth {:username "myusername" :password "mypassword"})]
  (println "Response Status:" (:status response))
  (println "Response Body:" (:body response)))
```

This will print the status code and the response body of the requested resource. You can also use other HTTP methods such as POST, PUT, and DELETE with basic authentication in a similar manner.

## Deep Dive

Basic authentication involves sending a username and password in the header of our HTTP request. The server then verifies this information before granting access to the requested resource. This type of authentication is considered to be less secure compared to others, as the username and password are sent in plain text and can be easily intercepted.

In order to improve security, it is recommended to use HTTPS along with basic authentication. This will encrypt the data being sent and make it more difficult for an attacker to intercept and access the sensitive information. It is also important to use strong usernames and passwords to further enhance security.

## See Also

- [clj-http library](https://github.com/dakrone/clj-http)
- [HTTP basic authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) 
- [HTTP methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)