---
title:                "Wysyłanie żądania http"
html_title:           "Swift: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiasz się, dlaczego potrzebujemy wysyłać żądania HTTP w naszych programach Swift? Odpowiedź jest prosta: żądania HTTP pozwalają nam na komunikację z serwerami i otrzymywanie informacji, takich jak dane, obrazy czy pliki.

## Jak to zrobić

Sending an HTTP request in Swift is a two-step process. First, we need to create a URL object representing the server we want to communicate with. Then, we use this URL object to create an HTTP request object, which we can then send to the server.

```Swift
// Step 1: Create URL object
guard let url = URL(string: "https://mywebsite.com") else {
  // handle URL creation failure
  return 
}

// Step 2: Create HTTP request object
var request = URLRequest(url: url)

// add HTTP method (ex. GET, POST, PUT)
request.httpMethod = "GET"

// set any additional headers if needed
request.setValue("application/json", forHTTPHeaderField: "Content-Type")

// send the request using URLSession
let task = URLSession.shared.dataTask(with: request) { data, response, error in
  guard error == nil else {
    // handle error
    return
  }
  
  // handle response from server (if needed)
  guard let data = data else { return }
  let responseString = String(data: data, encoding: .utf8)
  // do something with responseString
}
task.resume()
```

Deep Dive: Wysyłając żądanie HTTP, możemy także przesłać dane, np. JSON lub formularz HTTP, wraz z naszym zapytaniem. Wówczas należy ustawić odpowiedni nagłówek (w przykładzie ustawiliśmy `Content-Type` na `application/json`) oraz przekazać dane do ciała żądania.

## Zobacz też

- [Apple Developer Documentation: URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [Apple Developer Documentation: URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Podręcznik HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP)