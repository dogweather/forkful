---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

### What & Why?

Sending an HTTP request with Basic Authentication involves adding an 'Authorization' header to your request. The 'Authorization' header is calculated using Base64 encoding scheme. This lets you communicate securely with servers by verifying your identity. Programmers resort to this method to guarantee basic security while interacting with APIs or web services.

### How to:

For the task, we'll use the 'khttp' library. First, ensure you've installed it in your `build.gradle`:

```kotlin
dependencies {
  implementation 'khttp:khttp:0.1.0'
}
```

With that done, let's create a method to send a GET request with Basic Authentication:

```kotlin
import khttp.get

fun main(){
  val response = get("https://httpbin.org/basic-auth/user/passwd",
  	auth=("user" to "passwd")
)

  println(response.statusCode)
  println(response.text)
}
```

When you run the code, it sends a GET request to httpbin (a simple HTTP request/response service) and authenticates using 'user' and 'passwd' as 'username' and 'password' respectively. The response is then printed to the console. 

### Deep Dive

Historically, Basic Authentication was proposed as a stateless method to authenticate HTTP requests in 1999, as part of the HTTP/1.1 RFC 2617. Though, it shouldn't be used alone for sensitive information since it isn't encrypted and can be intercepted by malicious actors.

Alternatives include Digest Authentication, a more secure measure also proposed in RFC 2617, and token-based authentication, which is widely used today due to its scalability and simplicity.

The 'khttp' library simplifies the process by managing the encoding of username and password to Base64 and appending it to the 'Authorization' header. It hides the implementation detail, allowing you to focus on writing your own application logic.

### See Also:

1. ['khttp' library Github page](https://github.com/jkcclemens/khttp)
2. [HTTP Authentication RFC](https://www.ietf.org/rfc/rfc2617.txt)
3. [httpbin.org](http://httpbin.org/)
4. [Digest Authentication - Wikipedia](https://en.wikipedia.org/wiki/Digest_access_authentication)