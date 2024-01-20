---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP jest techniką umożliwiającą komunikację między klientem internetowym a serwerem. Programiści używają tego dla zdobycia danych z różnych serwisów webowych i API.

## Jak to zrobić:
W Swift wykorzystaj `URLSession` do wykonania żądania HTTP. Poniżej znajduje się proste zapytanie GET.

```Swift
import Foundation

let url = URL(string: "http://www.google.com")
let task = URLSession.shared.dataTask(with: url!) {(data, response, error) in
    if let error = error {
         print("Error: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Received data:\n\(str!)")
    }
}
task.resume()
```
Twój wynik będzie wyglądał tak:

```
Received data:
<!doctype html><html itemscope="" itemtype="http://schema.org/WebPage" ...
```

## Pogłębienie:
Zapytania HTTP są fundamentem komunikacji internetowej, a ich historia sięga wczesnych lat rozwoju sieci. W Pythonie, popularną alternatywą dla `URLSession` jest `requests`, w Node.js możemy użyć `axios` lub modułu `http`.

Kiedy wysyłamy żądanie HTTP, nawiązujemy połączenie TCP z serwerem i przesyłamy informacje w formacie tekstowym. Serwer odpowiada, również w formacie tekstowym. W Swift, `URLSession` zarządza tymi szczegółami dla nas, umożliwiając nam skupić się na obsłudze otrzymanych danych.

## Zobacz też:
1. [Podstawy URLSession](https://developer.apple.com/documentation/foundation/urlsession) - Dokumentacja Apple o URLSession.
2. [Wprowadzenie do HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP/Overview) - Obszerna analiza protokołu HTTP na MDN.
3. [Jak korzystać z HTTP w Swift](https://www.hackingwithswift.com/read/12/overview) - Przewodnik Hacking with Swift na temat wykonywania żądań HTTP w Swift.