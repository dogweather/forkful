---
title:                "Skicka en http-förfrågan"
html_title:           "Swift: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att kunna skicka HTTP-förfrågningar är en viktig del av att bygga moderna webbanvändargränssnitt. Genom att förstå hur man skickar förfrågningar kan du skapa interaktioner mellan ditt Swift-program och webbtjänster.

## Hur man gör

```Swift
let url = URL(string: "https://example.com")
let request = URLRequest(url: url!)

let task = URLSession.shared.dataTask(with: request) { data, response, error in
  if let error = error {
    print("Error: \(error)")
    return
  }
  guard let data = data, let response = response as? HTTPURLResponse else {
    print("Invalid response")
    return
  }
  print("Response status code: \(response.statusCode)")
  print("Response body: \(String(data: data, encoding: .utf8)!)")
}

task.resume()
```

Kodexemplet ovan visar hur du kan skicka en enkel HTTP GET-förfrågan med Swift. Genom att använda URLSession-klassen kan du skapa en anslutning till en URL och sedan hämta data från den. I detta exempel skriver vi också ut svaret från vår förfrågan, inklusive statuskoden och svarets innehåll.

## Djupdykning

Vid skickande av HTTP-förfrågningar finns det flera saker att tänka på, såsom vilken metod som ska användas (GET, POST, PUT, etc.), om några parametrar eller en payload behöver skickas med förfrågan, och hur din app ska hantera eventuella felmeddelanden från servern. Det är också viktigt att hantera eventuella säkerhetsmekanismer som autentisering eller tokenbaserad åtkomst.

En annan viktig aspekt att tänka på är att rikta in dig på asynkron kommunikation när du skickar HTTP-förfrågningar i Swift. Detta gör du genom att använda sig av URLSession och dess dataTask-metod, som vi använder i vårt kodexempel ovan. Detta är viktigt för att undvika att blockera huvudtråden för din app och förbättra prestandan.

## Se även

Här är några relaterade länkar för att hjälpa dig att utforska ämnet vidare:

- [Apple Developer Documentation for URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [A Beginner's Guide to URLSession in Swift](https://www.raywenderlich.com/960-urlsession-tutorial-getting-started)
- [HTTP Made Really Easy](https://www.jmarshall.com/easy/http/)