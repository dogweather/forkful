---
title:                "Envoi d'une requête HTTP avec authentification de base"
date:                  2024-01-20T18:02:40.069239-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"

category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Envoyer une requête HTTP avec une authentification de base consiste à fournir un nom d'utilisateur et un mot de passe pour accéder à une ressource web. Les programmeurs l'utilisent pour sécuriser l'accès aux API et aux services web.

## Comment faire :

```Swift
import Foundation

let username = "user"
let password = "pass"
let loginString = String(format: "%@:%@", username, password)
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()

var request = URLRequest(url: URL(string: "https://example.com/api/data")!)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let session = URLSession.shared
session.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {
        print(error?.localizedDescription ?? "No data")
        return
    }
    if let httpStatus = response as? HTTPURLResponse, httpStatus.statusCode != 200 {
        print("StatusCode should be 200, but is \(httpStatus.statusCode)")
        print("Response = \(response!)")
    }
    let responseData = String(data: data, encoding: .utf8)
    print("ResponseData: \(responseData!)")
}.resume()
```

Exemple de sortie :

```
ResponseData: {"data": "Données sécurisées"}
```

## Plongée en profondeur

Historiquement, l'authentification de base HTTP - inventée au début des années 90 - fait partie intégrante des standards d’Internet. Des alternatives modernes comprennent OAuth et les JWT (Jetons Web JSON) qui offrent une sécurité améliorée et une plus grande flexibilité. Une mise en œuvre correcte doit passer par la sécurisation de la connexion avec TLS/SSL, sinon les identifiants seraient exposés en clair sur internet. D'ailleurs, n'oubliez pas de stocker vos identifiants de manière sécurisée (pas en dur dans le code !).

## Voir également

- [Documentation URLSession Apple](https://developer.apple.com/documentation/foundation/urlsession)
- [RFC 7617 - 'The 'Basic' HTTP Authentication Scheme'](https://tools.ietf.org/html/rfc7617)
- [Comment mieux sécuriser ses données de connexion](https://www.owasp.org/index.php/Password_Storage_Cheat_Sheet)
