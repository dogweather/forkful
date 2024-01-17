---
title:                "Praca z json"
html_title:           "Swift: Praca z json"
simple_title:         "Praca z json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Co to jest JSON i dlaczego programiści go używają?
JSON (JavaScript Object Notation) jest popularnym formatem danych używanym do przechowywania i wymiany informacji między aplikacjami. Programiści często wykorzystują JSON, ponieważ jest on prosty w użyciu i ma dużo wszechstronnych zastosowań, takich jak komunikacja między klientem a serwerem lub przechowywanie ustawień aplikacji.

## Jak to zrobić:
Przykładowy kod poniżej pokazuje, jak można użyć biblioteki `JSONSerialization` w języku Swift, aby odczytać dane w formacie JSON z internetu oraz jak przetworzyć te dane do obiektów `Dictionary` i `Array`.

### Pobieranie danych JSON z internetu:
```Swift
if let url = URL(string: "https://example.com/mydata.json") {
    let task = URLSession.shared.dataTask(with: url, completionHandler: { (data, response, error) in
        // sprawdź, czy nie ma błędów
        if let error = error {
            print("Wystąpił błąd: \(error)")
            return
        }
        
        // przetwórz otrzymane dane do obiektu `Dictionary`
        if let data = data {
            do {
                guard let json = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] else {
                    print("Nie można przetworzyć danych JSON")
                    return
                }
                
                // można teraz odwołać się do kluczy lub wartości w obiekcie `json`
                let name = json["name"] as? String ?? "Nie znaleziono imienia"
                
                print("Witaj \(name)!")
                
            } catch {
                print("Wystąpił błąd: \(error)")
            }
        }
    })
    
    task.resume()
}
```

### Tworzenie obiektu JSON i wysłanie go do servera:
```Swift
let myObject: [String: Any] = [
  "name": "John",
  "age": 25,
  "hobbies": ["gaming", "reading"]
]

do {
  let jsonData = try JSONSerialization.data(withJSONObject: myObject, options: .prettyPrinted)
  let url = URL(string: "https://example.com/myapi")
  var request = URLRequest(url: url!)
  request.httpMethod = "POST"
  request.setValue("application/json", forHTTPHeaderField: "Content-Type")
  request.httpBody = jsonData

  let task = URLSession.shared.dataTask(with: request, completionHandler: { (data, response, error) in
      // sprawdź, czy nie ma błędów
      if let error = error {
          print("Wystąpił błąd: \(error)")
          return
      }
      
      // przetwórz otrzymane dane
      if let data = data {
        print("Dane zostały wysłane do serwera!")
      }
  })
  
  task.resume()
} catch {
  print("Wystąpił błąd: \(error)")
}
```

## Pogłębiona analiza:
JSON został stworzony jako alternatywa dla bardziej skomplikowanego formatu XML. Jest on bardzo podobny do notacji obiektowej w języku JavaScript, co czyni go łatwym do zrozumienia i implementacji w wielu językach programowania. Istnieje wiele innych bibliotek w języku Swift, które pozwalają na pracę z JSON, takich jak `Codable` czy `SwiftyJSON`.

## Zobacz również:
- [Oficjalna dokumentacja JSON w Swift](https://developer.apple.com/documentation/foundation/json_serialization)
- [Jak przetwarzać dane JSON w Swift z użyciem `Codable`](https://www.raywenderlich.com/3418439-encoding-and-decoding-in-swift-a-comprehensive-guide)