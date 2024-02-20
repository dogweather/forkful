---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:29.357441-07:00
description: "Het downloaden van een webpagina betekent het verkrijgen van gegevens\
  \ van het web en deze in je app brengen. Programmeurs doen dit om inhoud op te halen,\u2026"
lastmod: 2024-02-19 22:05:10.242327
model: gpt-4-0125-preview
summary: "Het downloaden van een webpagina betekent het verkrijgen van gegevens van\
  \ het web en deze in je app brengen. Programmeurs doen dit om inhoud op te halen,\u2026"
title: Een webpagina downloaden
---

{{< edit_this_page >}}

## Wat & Waarom?
Het downloaden van een webpagina betekent het verkrijgen van gegevens van het web en deze in je app brengen. Programmeurs doen dit om inhoud op te halen, met online diensten te interageren of gegevens te scrapen.

## Hoe:
Laten we `URLSession` gebruiken om de klus te klaren. Swift maakt het recht door zee.

```Swift
import Foundation

let url = URL(string: "https://www.example.com")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Fout:", error)
        return
    }

    if let httpResponse = response as? HTTPURLResponse, (200...299).contains(httpResponse.statusCode) {
        if let mimeType = httpResponse.mimeType, mimeType == "text/html",
           let data = data, let string = String(data: data, encoding: .utf8) {
            print("Gedownloade webpagina-inhoud:")
            print(string)
        } else {
            print("Ongeldig MIME-type of codering.")
        }
    } else {
        print("Server reageerde met fout.")
    }
}
task.resume()
// Zorg ervoor dat de speeltuin blijft lopen totdat de taak is voltooid
RunLoop.current.run()
```

Een voorbeelduitvoer ziet er mogelijk als volgt uit:

```
Gedownloade webpagina-inhoud:
<!doctype html>...
```

## Diepgaand
De `URLSession` API bestaat al sinds iOS 7 en macOS 10.9. Het was destijds een gamechanger, die de oudere, omslachtiger `NSURLConnection` verving. Hoewel `URLSession` krachtig en flexibel is, kun je voor meer complexe netwerkbehoeften ook overwegen derdenbibliotheken zoals Alamofire te gebruiken.

Bij implementatie, onthoud dat netwerkaanvragen asynchroon zijn. Dit betekent dat je app door kan gaan met andere taken terwijl de server op je terugkomt. Ook is het juist gebruiken van `URLSession` betrokken bij het sierlijk afhandelen van fouten en het controleren van de serverresponsstatus. Het controleren van het MIME-type is cruciaal om te zorgen dat je HTML ontvangt, en niet andere bestandstypen zoals JSON of een afbeelding.

## Zie Ook
Duik dieper of verken alternatieven:
- Apple's `URLSession` documentatie: [URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- Swift netwerken met Alamofire: [Alamofire](https://github.com/Alamofire/Alamofire)
- Swift async/await patroon voor `URLSession` in iOS 15+: [URLSession async/await](https://developer.apple.com/videos/play/wwdc2021/10054/)
