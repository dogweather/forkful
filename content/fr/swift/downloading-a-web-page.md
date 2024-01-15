---
title:                "Téléchargement d'une page web"
html_title:           "Swift: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi vous devriez télécharger une page web en utilisant Swift ? Eh bien, cela peut être utile pour extraire certaines données spécifiques d'un site ou pour automatiser certaines tâches, par exemple.

## Comment faire

Pour télécharger une page web en Swift, tout d'abord, vous devez importer la bibliothèque "Foundation" qui contient des fonctionnalités de réseau.

```Swift
import Foundation
```

Ensuite, vous pouvez utiliser la fonction "dataTask" du gestionnaire de session pour télécharger une page web. Par exemple, si nous voulons télécharger la page d'accueil de Google, nous pouvons utiliser cette fonction comme ceci :

```Swift
let url = URL(string: "https://www.google.com")!

let session = URLSession.shared

let task = session.dataTask(with: url) { data, response, error in
  if let error = error {
    print(error.localizedDescription)
    return
  }
                
  if let httpResponse = response as? HTTPURLResponse {
    print("Status code: \(httpResponse.statusCode)")
  }

  if let data = data {
    // Vous pouvez traiter les données ici
    print(data)
  }
}

task.resume()
```

Ce code va créer une URL à partir de la chaîne "https://www.google.com" et utiliser la fonction "dataTask" pour télécharger la page. Ensuite, il vérifiera le statut de la réponse et imprimera les données si elles sont téléchargées avec succès.

Il est important de noter que la fonction "dataTask" fonctionne de manière asynchrone, ce qui signifie qu'elle ne bloquera pas l'exécution de votre programme. Vous pouvez également spécifier différentes méthodes HTTP, des en-têtes de requête et des paramètres dans cette fonction pour personnaliser votre demande de téléchargement de page web.

## Plongée en profondeur

Il y a d'autres façons de télécharger des pages web en utilisant Swift, comme l'utilisation de la bibliothèque "Alamofire" ou la création d'une fonction personnalisée en utilisant la bibliothèque "URLSession". Vous pouvez également utiliser des bibliothèques tierces pour traiter les données téléchargées, telles que "SwiftSoup" pour l'analyse HTML.

Assurez-vous de comprendre les risques de télécharger du contenu web et de suivre les bonnes pratiques de sécurité lors de l'utilisation de données téléchargées dans votre application.

## Voir aussi

- [Documentation Apple pour URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Alamofire](https://github.com/Alamofire/Alamofire)
- [SwiftSoup](https://github.com/scinfu/SwiftSoup)