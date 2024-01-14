---
title:                "Swift: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Envoyer une requête HTTP est une compétence importante pour tout programmeur Swift. Cela vous permet de communiquer avec des serveurs distants et de récupérer des données pour alimenter vos applications. Apprenons comment le faire!

## Comment faire

Tout d'abord, vous devez créer une URL à partir de l'adresse du serveur que vous souhaitez contacter. Ensuite, vous créez une instance de `URLRequest` et spécifiez la méthode HTTP que vous souhaitez utiliser, par exemple GET ou POST. Enfin, vous pouvez utiliser la méthode `URLSession` pour envoyer la requête et recevoir la réponse.

```Swift
guard let url = URL(string: "https://www.example.com/api") else {
    print("Invalid URL")
    return
}

var request = URLRequest(url: url)
request.httpMethod = "GET"

let session = URLSession.shared
let task = session.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Error: \(error.localizedDescription)")
        return
    }

    if let httpResponse = response as? HTTPURLResponse {
        print("Status code: \(httpResponse.statusCode)")
    }

    if let data = data {
        print("Response: \(String(data: data, encoding: .utf8) ?? "No data")")
    }
}

task.resume()
```

Cela va envoyer une requête HTTP GET à l'URL spécifiée et afficher le code de statut de la réponse ainsi que les données récupérées.

## Plongée profonde

Il est important de noter que lorsque vous envoyez une requête HTTP, vous pouvez également inclure des informations supplémentaires dans l'en-tête de la requête, telles que des données d'authentification ou des paramètres spécifiques. Vous pouvez également spécifier le type de données que vous attendez en réponse, ce qui peut être utile si vous devez communiquer avec des serveurs utilisant différents formats de données.

Il est également possible de créer une requête HTTP à l'aide de la méthode `dataTask(with:completionHandler:)` de `URLSession` en fournissant des données au lieu d'une URL. Cela peut être utile si vous avez besoin de personnaliser complètement la requête envoyée.

## Voir aussi

- [Documentation officielle d'Apple sur l'envoi de requêtes HTTP](https://developer.apple.com/documentation/foundation/url_loading_system/sending_an_http_request)
- [Tutoriel sur l'envoi de requêtes HTTP en Swift](https://medium.com/swift-productions/swift-tutorial-how-to-make-a-simple-http-request-13277b128fd1)
- [Exemple de projet GitHub utilisant l'envoi de requêtes HTTP en Swift](https://github.com/fullsour/HTTP-Request-in-Swift)