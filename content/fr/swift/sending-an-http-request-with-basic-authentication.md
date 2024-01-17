---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Swift: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
L'envoi d'une requête HTTP avec une authentification de base est une méthode courante utilisée par les programmeurs pour sécuriser les échanges de données entre un client et un serveur. Cela implique l'utilisation d'un nom d'utilisateur et d'un mot de passe pour accéder à un site ou une application en ligne. Cela garantit que seules les personnes autorisées peuvent accéder aux données sensibles.

## Comment faire:
Voici un exemple simple en Swift pour envoyer une requête HTTP avec une authentification de base:

```Swift
let url = URL(string: "https://example.com")
let sessionConfig = URLSessionConfiguration.default
let credentials = "\(username):\(password)"
let base64Credentials = Data(credentials.utf8).base64EncodedString()
let authString = "Basic \(base64Credentials)"
sessionConfig.httpAdditionalHeaders = ["Authorization" : authString]

let session = URLSession(configuration: sessionConfig)

let task = session.dataTask(with: url!) { data, response, error in  
    guard let data = data, error == nil else {
        print("Error: \(String(describing: error))")
        return
    }
    
    // Traitement des données reçues ici
    
    if let httpStatus = response as? HTTPURLResponse, httpStatus.statusCode != 200 {
        print("Code d'état HTTP: \(httpStatus.statusCode)")
    }
}
task.resume()
```

L'authentification de base nécessite un encodage en base64 du nom d'utilisateur et du mot de passe avant d'être ajouté à l'en-tête de la requête HTTP. Ensuite, une session est créée avec une configuration qui inclut l'en-tête d'authentification. La session envoie ensuite une requête au serveur en utilisant l'URL fournie et traite les données renvoyées dans la fermeture.

## Plongée en profondeur:
L'authentification de base est un moyen simple mais pas très sécurisé de protéger les données échangées entre un client et un serveur. Elle a été introduite dans le protocole HTTP dans les années 1990 et a été remplacée par des méthodes plus sécurisées telles que OAuth. Avec l'avancée de la sécurité en ligne, il est fortement recommandé d'utiliser des méthodes plus avancées pour protéger les données sensibles.

## Voir aussi:
Pour en savoir plus sur l'authentification de base en Swift et d'autres méthodes d'authentification, consultez la documentation officielle d'Apple sur les URLSessions et la sécurité en ligne.