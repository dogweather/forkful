---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "Swift: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
## Hur man
## Djupdykning

När du utvecklar en iOS-app eller en webbapplikation, kan du behöva kommunicera med en extern server för att hämta eller skicka data. Ofta används HTTP-protokollet för att skicka förfrågningar till servern. Ibland kan dock denna kommunikation behöva vara säker och därför används basic authentication för att skydda den. I denna artikel kommer vi att djupdyka i hur man kan skicka en HTTP-förfrågan med basic authentication i Swift.

### Steg 1: Importera nödvändiga bibliotek
Först och främst behöver vi importera nödvändiga bibliotek för att kunna skicka en HTTP-förfrågan. I detta fall kommer vi att importera `Foundation` för att använda `URLSession` för att skicka förfrågan och få svar från servern.

```Swift
import Foundation
```

### Steg 2: Skapa en URL och en förfrågningsbegäran
Nästa steg är att skapa en URL för vår förfrågan. Detta är den webbadress som pekar på den externa servern som vi vill kommunicera med.

```Swift
// Här anger du din serveradress
guard let url = URL(string: "https://www.example.com") else {
    fatalError("Ogiltig URL")
}
```

Sedan behöver vi skapa en förfrågningsbegäran med den skapade URL:en. I denna begäran kan vi ställa in metoden (GET, POST, etc.), önskat svarstyp och eventuella body- eller headerparametrar.

```Swift
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("application/json", forHTTPHeaderField: "Accept")
```

### Steg 3: Lägg till basic authentication
För att lägga till basic authentication i vår förfrågningsbegäran behöver vi först skapa en användarautentisering i form av en användarnamn och lösenordskombination.

```Swift
let username = "användarnamn"
let password = "lösenord"
```

Vi kan sedan använda dessa uppgifter för att skapa en autentiseringssträng med formatet "användarnamn:lösenord" och omvandla den till Base64-kodning.

```Swift
let loginString = "\(username):\(password)"

if let loginData = loginString.data(using: .utf8) {
    let base64LoginString = loginData.base64EncodedString()
    request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
}
```

Nu är vår förfrågningsbegäran förberedd med basic authentication och vi kan skicka den till servern.

### Steg 4: Skicka förfrågan och hämta svar
Det sista steget är att använda `URLSession` för att skicka vår förfrågningsbegäran och hämta svar från servern. Vi skapar en `URLSession`-instans och använder `dataTask` för att skicka förfrågan.

```Swift
URLSession.shared.dataTask(with: request) { (data, response, error) in
    // Här kan vi arbeta med svaret från servern
}.resume()
```

En vanlig användning av svaret skulle vara att hämta en JSON-fil och sedan omvandla den till en Swift-struktur med hjälp av `JSONDecoder`.

```Swift
if let data = data {
    do {
        let decoder = JSONDecoder()
        let result = try decoder.decode(Result.self, from: data)
        // Här kan vi arbeta med den omvandlade datan
    } catch {
        print(error)
    }
}
```

## Djupdykning
Som du kan se är det ganska enkelt att lägga till basic authentication i en HTTP-förfrågan i Swift. Några saker att tänka på är att du bör hantera eventuella felmeddelanden och hantera säkerheten för din användarautentisering.

Det finns också andra sätt att skicka en HTTP-förfrågan med basic authentication, såsom att