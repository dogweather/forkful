---
date: 2024-01-20 17:45:12.233185-07:00
description: "Jak to zrobi\u0107: Aby pobra\u0107 stron\u0119 internetow\u0105 w Swift,\
  \ u\u017Cyjemy URLSession, kt\xF3ry jest cz\u0119\u015Bci\u0105 standardowej biblioteki\
  \ Foundation. Poni\u017Cej znajdziesz\u2026"
lastmod: '2024-03-13T22:44:35.753847-06:00'
model: gpt-4-1106-preview
summary: "Aby pobra\u0107 stron\u0119 internetow\u0105 w Swift, u\u017Cyjemy URLSession,\
  \ kt\xF3ry jest cz\u0119\u015Bci\u0105 standardowej biblioteki Foundation."
title: Pobieranie strony internetowej
weight: 42
---

## Jak to zrobić:
Aby pobrać stronę internetową w Swift, użyjemy URLSession, który jest częścią standardowej biblioteki Foundation. Poniżej znajdziesz przykładowy kod:

```Swift
import Foundation

func downloadWebPage(url: URL) {
    let session = URLSession(configuration: .default)
    let task = session.dataTask(with: url) { data, response, error in
        if let error = error {
            print("Wystąpił błąd: \(error)")
        } else if let data = data {
            if let pageContent = String(data: data, encoding: .utf8) {
                print(pageContent)
            }
        }
    }
    task.resume()
}

// Użyj tej funkcji tak:
if let url = URL(string: "http://example.com") {
    downloadWebPage(url: url)
}
```

Przykładowe wyjście to zawartość strony http://example.com wydrukowana w konsoli.

## Dogłębna analiza:
Zanim URLSession stał się standardem, programiści używali NSURLConnection, lecz URLSession wprowadził bardziej elastyczne i wydajne API. Alternatywnie, do pobierania stron można używać bibliotek trzecich, takich jak Alamofire, które zapewniają dodatkową funkcjonalność i uprośczenie kodu.

Kiedy używasz URLSession, musisz pamiętać o kilku szczegółach implementacyjnych, jak obsługa błędów czy prawidłowe zarządzanie sesjami i zadania. Pobranie strony to proces asynchroniczny, więc ważne jest, aby kod, który ma wykonać po pobraniu (np. aktualizacja UI), znalazł się w odpowiednim closure’ze.

## Zobacz też:
- [Dokumentacja URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Alamofire](https://github.com/Alamofire/Alamofire)
