---
title:                "Swift: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi
Les requêtes HTTP avec une authentification basique sont souvent utilisées pour sécuriser l'accès à des informations sensibles telles que des données utilisateur ou des données financières. Elles permettent également de différencier les différents utilisateurs et de limiter l'accès à certaines ressources.

## Comment Faire
La première étape pour envoyer une requête HTTP avec une authentification basique est de créer une URL avec la ressource cible. Ensuite, vous devez créer une instance de URLRequest en utilisant cette URL et spécifier la méthode de requête, qui est généralement "GET". Enfin, vous devez ajouter un en-tête d'authentification à votre requête en utilisant le nom d'utilisateur et le mot de passe appropriés. Voici un exemple de code pour effectuer cette tâche en utilisant Swift :

```Swift
let url = URL(string: "https://www.example.com/api/users")
let request = URLRequest(url: url!)
request.httpMethod = "GET"

let userName = "John"
let password = "secret"
if let headers = request.allHTTPHeaderFields {
    request.allHTTPHeaderFields = headers
} else {
    request.allHTTPHeaderFields = [:]
}
guard let userNameData = userName.data(using: .utf8) else {
    fatalError("Failed to convert username to Data.")
}
guard let passwordData = password.data(using: .utf8) else {
    fatalError("Failed to convert password to Data.")
}
let userPasswordString = "\(userNameData.base64EncodedString()):\(passwordData.base64EncodedString())"
request.setValue("Basic \(userPasswordString)", forHTTPHeaderField: "Authorization")
```

Après avoir créé et configuré votre requête, vous pouvez l'envoyer en utilisant une instance de URLSession. Voici un exemple de code pour effectuer cette tâche :

```Swift
let session = URLSession(configuration: .default)
let task = session.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {
        print(error?.localizedDescription ?? "No data received")
        return
    }
    if let response = response as? HTTPURLResponse {
        print(response.statusCode)
    }
    print(String(data: data, encoding: .utf8)!)
}
task.resume()
```

Lorsque la tâche est terminée, vous devriez recevoir une réponse avec le statut "200" si votre requête a réussi. Vous pouvez également utiliser les données reçues de la requête pour effectuer des opérations supplémentaires.

## Plongée Profonde
L'utilisation d'une authentification basique pour les requêtes HTTP peut sembler simple, mais elle peut en fait présenter certaines vulnérabilités en matière de sécurité, notamment le risque de réutilisation incorrecte des noms d'utilisateur et des mots de passe stockés dans le code. Pour éviter ces risques, il est important d'utiliser des techniques de sécurité supplémentaires telles que le hachage des mots de passe et la rotation fréquente des noms d'utilisateur et des mots de passe.

## Voir Aussi
- [Apple Developer Documentation - URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Tutorial de requêtes HTTP avec Swift](https://medium.com/@nimjea/http-requests-with-swift-3-fda8e70ae079)
- [Utilisation de l'authentification basique pour les requêtes API REST avec Swift](https://morioh.com/p/fde899d7a6b9)