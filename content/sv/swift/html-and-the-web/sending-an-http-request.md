---
title:                "Skicka en http-förfrågan"
aliases:
- /sv/swift/sending-an-http-request/
date:                  2024-01-20T18:00:46.857406-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en http-förfrågan"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran är hur din app pratar med en webbserver: att begära data eller skickar information. Programmerare gör detta för att interagera med webbaserade tjänster, hämta uppdaterat innehåll eller posta användardata.

## How to:
Använd `URLSession` för att hantera nätverkskommunikation. Här är ett enkelt exempel på att göra en GET-förfrågan till en JSON-API.

```Swift
import Foundation

let url = URL(string: "https://api.exempel.se/data")!
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let error = error {
        print("Ett fel uppstod: \(error)")
        return
    }
    
    if let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200 {
        if let data = data, 
           let jsonData = try? JSONSerialization.jsonObject(with: data) {
            print("Data mottagen: \(jsonData)")
        }
    } else {
        print("Servern svarade med fel statuskod")
    }
}

task.resume()
```

Förventad output är JSON-data som skrivs ut till konsolen, förutsatt att servern svarar korrekt.

## Deep Dive
HTTP-begäran är en grundläggande del av webben; den definierades på 90-talet och är fortfarande kärnan i webbkommunikation. Det finns olika typ av förfrågningar – som GET, POST, PUT och DELETE – för olika syften. Alternativ till `URLSession` inkluderar tredjepartsbibliotek som Alamofire som erbjuder mer funktionalitet med mindre kod. `URLSession` är dock inbyggt och kraftfullt nog för de flesta behov, och det fungerar direkt med Swift kod utan externa beroenden.

## See Also
För mer detaljerad information om hur du använder `URLSession`, se Apples dokumentation:
- [URLSession](https://developer.apple.com/documentation/foundation/urlsession)

Titta även på Alamofire för ett kraftfullt alternativ till `URLSession`:
- [Alamofire GitHub](https://github.com/Alamofire/Alamofire)
