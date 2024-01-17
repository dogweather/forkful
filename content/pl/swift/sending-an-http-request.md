---
title:                "Wysyłanie żądania http."
html_title:           "Swift: Wysyłanie żądania http."
simple_title:         "Wysyłanie żądania http."
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądań HTTP jest niezbędnym elementem dla programistów tworzących aplikacje korzystające z sieci. Jest to proces, w którym aplikacja przesyła żądanie do serwera, który odpowiada zwracając dane lub wykonując żądane działanie. Programiści wysyłają HTTP żądania, aby uzyskać dostęp do zasobów online lub skomunikować się z innymi serwisami lub aplikacjami.

## Jak to zrobić?

Aby wysłać HTTP żądanie w Swift, należy użyć biblioteki URLSession, która jest wbudowana w język. Można utworzyć nowe żądanie, ustawić jego wymagane parametry, takie jak adres URL i metoda żądania, a następnie wysłać je do serwera. Poniżej znajduje się przykładowy kod:

```swift
let url = URL(string: "https://www.example.com")!
let request = URLRequest(url: url)
let task = URLSession.shared.dataTask(with: request) { data, response, error in
  if let error = error {
    print("Błąd: \(error)")
    return
  }
  guard let data = data else {
    print("Nie ma żadnych danych zwracanych przez serwer.")
    return
  }
  
  // Przetworzenie danych odpowiedzi
  
  if let response = response as? HTTPURLResponse {
    print("Serwer zwrócił kod odpowiedzi: \(response.statusCode)")
  }
}
task.resume()
```

## Deep Dive

Wysyłanie HTTP żądań jest niezbędnym elementem dla korzystania z internetu przez aplikacje. Wcześniej, przed powstaniem protokołu HTTP w 1989 roku, komunikacja między aplikacjami w sieci była utrudniona i niestabilna. Obecnie istnieją również inne technologie do wysyłania żądań sieciowych, takie jak WebSocket czy gniazda TCP/IP, ale HTTP jest w dalszym ciągu powszechnie stosowanym protokołem dla aplikacji internetowych.

## See Also

- [Dokumentacja URLSession w Swift](https://developer.apple.com/documentation/foundation/urlsession)
- [Porównanie protokołów sieciowych w Swift](https://www.raywenderlich.com/9933509-urlsession-tutorial-getting-started)
- [Tutorial: Wysyłanie HTTP żądań w Swift](https://www.hackingwithswift.com/read/32/overview)