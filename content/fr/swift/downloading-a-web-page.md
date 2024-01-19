---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Télécharger une page web signifie récupérer ses données pour les utiliser dans votre application. Ce procédé est habituellement utilisé par les programmeurs pour obtenir des informations depuis des sources externes en ligne.

## Comment faire:

La bibliothèque Foundation de Swift fournit une classe URLSession pour gérer le téléchargement. Voici un exemple de téléchargement simple de page web.

```Swift
import Foundation

if let url = URL(string: "https://www.exemple.fr") {
    let urlSession = URLSession.shared
    let task = urlSession.dataTask(with: url) { (data, response, error) in
        if let er = error {
            print("Erreur: \(er)")
        }
        else if let incomingData = data { 
            let dataString = String(data: incomingData, encoding: .utf8)
            print("Page Web: \(dataString ?? "Pas de données")")
        }
    }
    task.resume()
}
```

Dans cet exemple, si l'URL est valide, on crée une URLSession pour gérer la requête HTTP. La méthode dataTask(with:) initialise une tâche à effectuer une requête HTTP GET. Si aucun problème n'est rencontré, les données de la page web sont affichées.

## Plongeons plus loin:

La méthode que nous venons de voir utilise la fonction de retour d'appel async pour traiter la réponse HTTP. Si nous remontons dans le temps, dimanche 7 novembre 2021, Apple a publié Swift 5.5 qui introduit les mots-clés async et await, simplifiant davantage le code asynchrone.

Cependant, il existe d'autres alternatives comme Alamofire pour le téléchargement web, qui est une bibliothèque HTTP de haut niveau. C'est une bonne alternative lorsqu'il y a besoin de fonctionnalités avancées, comme la prise en charge des multi-parties, des en-têtes HTTP personnalisés, des paramètres de requêtes et des téléchargements/cachage d'images.

En implémentant le téléchargement d'une page web, il est essentiel de maintenir une gestion adéquate des erreurs et une sécurité robuste. Par exemple, toujours vérifier les erreurs possibles retournées par dataTask(with:).

## Voir également:

Pour en apprendre davantage sur URLSession, consultez la [documentation officielle de Apple](https://developer.apple.com/documentation/foundation/urlsession). 

Pour un guide de débutant sur Swift, visitez [https://developer.apple.com/swift/](https://developer.apple.com/swift/).

Alamofire a des resources utiles ici: [GitHub - Alamofire](https://github.com/Alamofire/Alamofire).

Il est toujours bénéfique de savoir plus sur le déroulement sous le capot des requêtes HTTP, [MDN Web Docs offre une excellente explication.](https://developer.mozilla.org/en-US/docs/Web/HTTP)