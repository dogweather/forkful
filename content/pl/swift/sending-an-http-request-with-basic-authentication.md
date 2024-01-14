---
title:                "Swift: Wysyłanie żądania http z autoryzacją podstawową"
simple_title:         "Wysyłanie żądania http z autoryzacją podstawową"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz komunikować się z innymi serwerami lub usługami internetowymi w swojej aplikacji Swift, musisz wysyłać żądania HTTP. W niektórych przypadkach musisz również uwierzytelnić się za pomocą podstawowej autoryzacji. W tym artykule dowiesz się, dlaczego jest to niezbędne i jak to zrobić.

## Jak to zrobić

Aby wysłać żądanie HTTP z uwierzytelnieniem podstawowym w Swift, musisz użyć klasy URLSession i jej metody dataTask przyjmującej obiekt URLRequest. URLRequest zawiera wszystkie informacje o żądaniu, w tym adres URL i dane uwierzytelniające. Kod wyglądałby mniej więcej tak:

```Swift
let url = URL(string: "https://www.example.com")
var request = URLRequest(url: url!)
let username = "username"
let password = "password"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: .utf8)
let base64LoginData = loginData!.base64EncodedString()

request.setValue("Basic \(base64LoginData)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Błąd: \(error)")
    } else {
        if let httpResponse = response as? HTTPURLResponse {
            print("Status kod: \(httpResponse.statusCode)")
        }
        if let data = data, let dataString = String(data: data, encoding: .utf8) {
            print("Odpowiedź: \(dataString)")
        }
    }
}

task.resume()
```

Output powinien zawierać status kod żądania oraz odpowiedź serwera. W przypadku autoryzacji niepowodzeniem, można otrzymać błąd "401 Unauthorized".

## Deep Dive

Podstawowa autoryzacja w HTTP polega na przesyłaniu danych uwierzytelniających w nagłówku "Authorization" w formacie "Basic base64(username:password)". Jest to szyfrowanie base64, które jest jednakie dla każdej próby uwierzytelnienia i nie jest uważane za bezpieczne. Dlatego zaleca się, aby używać uwierzytelnienia z tokenem API lub bardziej złożonych metod uwierzytelniania, szczególnie jeśli aplikacja zawiera wrażliwe dane użytkownika.

## Zobacz również

- [Dokumentacja Apple: URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Tutorial: Praca z HTTP w Swift](https://www.hackingwithswift.com/read/35/3/the-ultimate-guide-to-http-requests-in-swift)
- [Poradnik: Uwierzytelnianie w Swift z użyciem kluczy API](https://www.raywenderlich.com/482-apple-s-keychain-framework-a-tutorial)