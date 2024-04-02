---
date: 2024-01-20 18:00:55.288046-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP to spos\xF3b, aby tw\xF3j program\
  \ porozmawia\u0142 z serwerem: zapyta\u0142 o dane, przes\u0142a\u0142 formularz\
  \ albo pobi\u0142 witryn\u0119. Programi\u015Bci robi\u0105 to,\u2026"
lastmod: '2024-03-13T22:44:35.751793-06:00'
model: gpt-4-1106-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP to spos\xF3b, aby tw\xF3j program\
  \ porozmawia\u0142 z serwerem: zapyta\u0142 o dane, przes\u0142a\u0142 formularz\
  \ albo pobi\u0142 witryn\u0119. Programi\u015Bci robi\u0105 to,\u2026"
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

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
