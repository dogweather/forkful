---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pobieranie strony internetowej polega na tym, że za pomocą pewnego kodu programu uzyskujemy dostęp do treści strony internetowej i przechowujemy ją w swoim systemie. Programiści robią to, aby analizować dane, szukać błędów, a nawet budować aplikacje opierające się na tych stronach.

## Jak to zrobić:

Zobaczmy, jak zrealizować to za pomocą Swifta. Poniżej przedstawiam prosty kod do pobrania strony internetowej.

```Swift
import Foundation

let url = URL(string: "https://www.mojastrona.pl")!
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let error = error {
        print("Wystąpił błąd: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Dane pobrane: \n\(str ?? "")")
    }
}

task.resume()
```

Po uruchomieniu tego kodu, spodziewane wyjście powinno wyglądać tak:

```Swift
Dane pobrane: 
<!DOCTYPE html>
<html>
<head>
    <title>Moja Strona</title>
...
```

## Dogłębne spojrzenie:

Pobieranie stron internetowych ma długą historię, wynikającą z rozwoju internetu i potrzeby interakcji z istniejącymi stronami internetowymi. W Swift, używamy klasy `URLSession`, która jest częścią Foundation framework, ale są też alternatywne metody, takie jak `Alamofire` czy `Moya`, które mogą zaoferować bardziej zaawansowane funkcje. Co więcej, chociaż pokazujemy tu bardzo prosty przykład, na produkcje, musisz pamiętać o obsłudze błędów i making requests asynchronously, aby nie blokować głównego wątku użytkownika.

## Zobacz także:

1. Dokumentacja URLSession: [https://developer.apple.com/documentation/foundation/urlsession](https://developer.apple.com/documentation/foundation/urlsession)
2. Alamofire: [https://github.com/Alamofire/Alamofire](https://github.com/Alamofire/Alamofire)
3. Moya: [https://github.com/Moya/Moya](https://github.com/Moya/Moya)