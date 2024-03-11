---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:11.732888-07:00
description: "Een HTTP-verzoek verzenden gaat over het aankloppen bij de deur van\
  \ een webserver, om gegevens te vragen of aan te bieden. Programmeurs doen dit om\
  \ te\u2026"
lastmod: '2024-03-11T00:14:24.991482-06:00'
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek verzenden gaat over het aankloppen bij de deur van een\
  \ webserver, om gegevens te vragen of aan te bieden. Programmeurs doen dit om te\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?
Een HTTP-verzoek verzenden gaat over het aankloppen bij de deur van een webserver, om gegevens te vragen of aan te bieden. Programmeurs doen dit om te interageren met API's, inhoud te downloaden of te communiceren met andere diensten.

## Hoe:

Swift maakt het eenvoudig om HTTP-verzoeken te verzenden met behulp van de `URLSession` klasse. Hier is een eenvoudig voorbeeld van een GET-verzoek:

```Swift
import Foundation

// URL van de bron die je aanvraagt
if let url = URL(string: "https://api.example.com/data") {

    // Maak een URLSessionDataTask aan
    let task = URLSession.shared.dataTask(with: url) { data, response, error in
        
        // Controleer op een fout
        if let error = error {
            print("Fout bij het ophalen van gegevens: \(error)")
            return
        }
        
        // Controleer of we een geldige reactie en gegevens hebben ontvangen
        if let httpResponse = response as? HTTPURLResponse, 
           httpResponse.statusCode == 200,
           let data = data {
            
            // Zet gegevens om naar string en print
            let dataString = String(decoding: data, as: UTF8.self)
            print(dataString)
        }
    }
    // Start de taak
    task.resume()
}

// Voorbeelduitvoer zou de inhoud zijn die van de API is opgehaald.
```

Om een POST-verzoek met JSON te verzenden:

```Swift
import Foundation
import CoreFoundation

// Je API-eindpunt
if let url = URL(string: "https://api.example.com/submit") {

    // Bereid de gegevens voor die je wilt verzenden
    let dictionary = ["key": "value"]
    guard let jsonData = try? JSONSerialization.data(withJSONObject: dictionary) else {
        print("Fout: Kan geen JSON maken van dictionary")
        return
    }
    
    // Bereid URLRequest voor
    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    request.setValue("application/json", forHTTPHeaderField: "Content-Type")
    request.httpBody = jsonData
    
    // Maak en start de taak
    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        // Verwerk de reactie hier
    }
    task.resume()
}

// Uitvoer is afhankelijk van de reactie van de server. Geen standaarduitvoer.
```

## Diepgaande duik:
HTTP-verzoeken zijn het brood en de boter van webcommunicatie. Ze bestaan al sinds de prille begindagen van het web en bieden een gestandaardiseerde manier van gegevensuitwisseling.

Alternatieven voor `URLSession` omvatten bibliotheken van derden zoals Alamofire die de syntaxis vereenvoudigen en functionaliteit toevoegen. Echter, `URLSession` blijft de native keuze voor netwerkaanroepen en Apple houdt het up-to-date met de nieuwste netwerkfuncties en beveiligingsnormen.

Een implementatiedetail om op te merken is dat netwerkaanvragen van nature asynchroon zijn in Swift. Ze worden op de achtergrond uitgevoerd, waardoor de app responsief kan blijven. Wanneer een reactie terugkomt, wordt een voltooiingshandler aangeroepen. Het is cruciaal om het beheer van threads goed te hanteren, vooral bij het bijwerken van de UI, wat op de hoofdthread moet gebeuren.

## Zie ook:

- [URLSession | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [Werken met JSON in Swift](https://developer.apple.com/swift/blog/?id=37)
- [Alamofire GitHub Repository](https://github.com/Alamofire/Alamofire)
