---
title:                "Envoi d'une requête HTTP"
date:                  2024-01-20T18:00:41.284427-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Envoyer une requête HTTP, c'est comme expédier une lettre sur Internet pour récupérer des données ou interagir avec des services. Les devs le font pour communiquer avec des serveurs web, alimenter leurs apps avec des infos fraîches, ou encore pour pousser des données à traiter.

## How to:
### Envoyer une requête GET simple
```Swift
import Foundation
import UIKit

let url = URL(string: "https://api.example.com/data")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Erreur de requête: \(error)")
        return
    }
    
    guard let httpResponse = response as? HTTPURLResponse, (200...299).contains(httpResponse.statusCode) else {
        print("Réponse serveur non valide.")
        return
    }
    
    if let mimeType = httpResponse.mimeType, mimeType == "application/json",
       let data = data {
        // Traitez ici vos données JSON...
        print("Données reçues: \(data)")
    }
}
task.resume()
```

### Résultat attendu
```
Données reçues: <...data in some format...>
```

## Deep Dive
HTTP est le fondement du web. Créé au début des années 90, il permet l'échange d'infos entre clients et serveurs. Swift utilise `URLSession` pour faire des requêtes réseau. C'est puissant, flexible et adapté pour iOS, macOS, watchOS, et tvOS. 

Outre GET, vous avez POST, PUT, DELETE, etc. Chacun a son rôle pour créer, lire, mettre à jour et supprimer des données. En Swift, vous pouvez customiser les requêtes à fond – timeout, en-têtes HTTP, gestion de la session, et plus encore.

`URLSession` est un remplaçant au `NSURLConnection` déprécié. Swift propose aussi des alternatives modernes telles que les frameworks Alamofire ou Moya. Ces frameworks simplifient souvent le code réseau, mais `URLSession` reste canon.

## See Also
- Documentation officielle de URLSession: [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- Alamofire, un HTTP networking library pour Swift: [Alamofire on GitHub](https://github.com/Alamofire/Alamofire)
- Article sur les patterns de conception réseau en Swift: [Ray Wenderlich - Networking](https://www.raywenderlich.com/35-networking-in-swift-with-urlsession)
