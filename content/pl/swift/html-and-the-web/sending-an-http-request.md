---
title:                "Wysyłanie żądania HTTP"
aliases:
- /pl/swift/sending-an-http-request/
date:                  2024-01-20T18:00:55.288046-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)

Wysyłanie żądania HTTP to sposób, aby twój program porozmawiał z serwerem: zapytał o dane, przesłał formularz albo pobił witrynę. Programiści robią to, żeby aplikacje mogły wchodzić w interakcje z internetem – wymieniać dane, synchronizować informacje, cokolwiek "online" znaczy dla twojego kodu.

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
