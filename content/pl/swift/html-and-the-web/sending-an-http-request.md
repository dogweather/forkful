---
date: 2024-01-20 18:00:55.288046-07:00
description: "How to (Jak to zrobi\u0107): W dawnych czasach, wysy\u0142anie \u017C\
  \u0105dania HTTP wymaga\u0142o linijk\xF3w w terminalu z `curl` albo komplikacji\
  \ z `sockets`. Teraz, Swift\u2026"
lastmod: '2024-04-05T22:50:50.089362-06:00'
model: gpt-4-1106-preview
summary: "W dawnych czasach, wysy\u0142anie \u017C\u0105dania HTTP wymaga\u0142o linijk\xF3\
  w w terminalu z `curl` albo komplikacji z `sockets`."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

## How to (Jak to zrobić):
```Swift
import Foundation

let url = URL(string: "https://api.example.com/data")!
var request = URLRequest(url: url)
request.httpMethod = "GET"

let task = URLSession.shared.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Client error: \(error.localizedDescription)")
    }

    guard let httpResponse = response as? HTTPURLResponse,
          (200...299).contains(httpResponse.statusCode) else {
        print("Server error")
        return
    }

    if let mimeType = httpResponse.mimeType, mimeType == "application/json",
       let data = data {
        do {
            let json = try JSONSerialization.jsonObject(with: data, options: [])
            print("Response: \(json)")
        } catch {
            print("JSON error: \(error.localizedDescription)")
        }
    }
}

task.resume()
```

Sample output:
```
Response: {
    id = 1;
    name = "Example Data";
}
```

## Deep Dive (Dogłębna analiza):
W dawnych czasach, wysyłanie żądania HTTP wymagało linijków w terminalu z `curl` albo komplikacji z `sockets`. Teraz, Swift załatwia ci większość pracy – używasz `URLSession` do zarządzania sesjami sieciowymi.

Alternatywy? Biblioteki jak `Alamofire` upiększają kod i dodają dodatkową funkcjonalność. Da się też używać niższego poziomu `CFNetwork` dla większej kontroli. Wybór zależy od potrzeb.

Implementacja? `URLRequest` tworzy twoje żądanie, ustalając metodę, nagłówki, ciało. `URLSession` obsługuje wysyłkę i odbiór danych. Można synchronicznie (`dataTask`) czy asynchronicznie (`dataTask(with:completionHandler:)`) – prawdziwa moc Swifta.

## See Also (Zobacz również):
- [Swift’s URLSession Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [Alamofire GitHub Repository](https://github.com/Alamofire/Alamofire)
