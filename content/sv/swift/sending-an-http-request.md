---
title:                "Swift: Sända en http-request"
simple_title:         "Sända en http-request"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför
Att skicka en HTTP-begäran är en viktig del av att skapa en app som interagerar med internet. Det gör det möjligt för din app att kommunicera med servrar och hämta data som behövs för att presentera för användaren.

## Så här gör du
För att skicka en HTTP-begäran i Swift, behöver du använda dig av URLSession-klassen. Först behöver du skapa en URL-instans som representerar den resurs du vill hämta. Sedan skapar du en URLRequest-instans med hjälp av den skapade URL:en. Slutligen skapar du en URLSessionDataTask där du kan använda URLRequest-instansen för att skicka begäran till servern. Nedan är ett exempel som hämtar data från en API-tjänst och skriver ut det till konsolen:

```Swift
// Skapa URL-instans baserat på en given adress
let url = URL(string: "https://example.com/api")

// Skapa URLRequest-instans med hjälp av URL:en
var request = URLRequest(url: url!)

// Sätt metod till GET (kan också vara POST, PUT eller DELETE)
request.httpMethod = "GET"

// Skapa URLSession och URLSessionDataTask
let session = URLSession.shared
let task = session.dataTask(with: request) { data, response, error in
    // Kontrollera om det finns några fel och hämta data om det inte finns något fel
    if let error = error {
        print("Något gick fel: \(error.localizedDescription)")
    } else if let data = data {
        // Skriv ut data som en sträng
        let dataAsString = String(data: data, encoding: .utf8)!
        print(dataAsString)
    }
}

// Starta uppgiften
task.resume()
```

Output:
```
Detta är vad som hämtades från API-tjänsten.
```

## Deep Dive
Det finns många olika parametrar du kan sätta för din URLRequest-instans, till exempel HTTP-headers, timeout, cachning och autentisering. Du kan också skicka data i en begäran genom att sätta body-egenskapen på URLRequest-instansen och ange data som behövs för begäran. Det finns också möjligheter att hantera eventuella fel som kan uppstå vid en begäran genom att använda sig av delegate-mönstret i URLSession-klassen. Det finns även andra sätt att skicka HTTP-begäran på, till exempel med hjälp av en tredjepartsbibliotek eller genom att skriva anpassad kod, men grundprinciperna är samma.

## Se även
- [Apple Developer Documentation – URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Pieter Omvlee – A Beginner’s Guide to URLSession in Swift](https://blog.usejournal.com/a-beginners-guide-to-urlsession-in-swift-5-aeda7ae9a2b8)
- [Ray Wenderlich – URLSession Tutorial](https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started)