---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Une requête HTTP est une demande que l'on fait à un serveur web d'envoyer des données. Les programmeurs en ont besoin pour obtenir ou manipuler des informations sur le web à partir de leurs applications.

## Comment faire :

Voici un exemple de requête HTTP GET en Swift avec URLSession. Il récupère des données à partir d'une URL et les imprime.

```Swift
import Foundation

let url = URL(string: "https://apieexemple.com")!
let task = URLSession.shared.dataTask(with: url) {(data, response, error) in
    if let error = error {
        print("Erreur: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Reçu: \(str)")
    }
}
task.resume()
```

Lorsque cette URL est accessible, voici une sortie possible :
```
Reçu: {"message":"Hello, world!"}
```

## Plongée en profondeur

Historiquement, l'envoi de requêtes HTTP a été une partie essentielle de la communication entre les clients et les serveurs Web. Cette pratique remonte à la création du protocole HTTP en 1991.

Concernant les alternatives, vous pouvez utiliser d'autres bibliothèques Swift comme Alamofire pour un style plus orienté syntaxe Swift ou des outils de niveau plus bas comme `libcurl`.

Le détail de mise en œuvre : URLSession de Foundation en Swift utilise des tâches (tasks) pour gérer les requêtes HTTP. Si vous utilisez Alamofire, il utilise URLSession en dessous, mais fournit une API à la syntaxe plus conviviale.

## Voir aussi

- La [documentation Apple sur URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- Pour une approche de plus haut niveau, regardez [Alamofire](https://github.com/Alamofire/Alamofire) sur GitHub.