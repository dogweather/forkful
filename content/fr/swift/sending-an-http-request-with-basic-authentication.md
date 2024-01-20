---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

L'envoi d'une requête HTTP avec une authentification de base est le processus de transmission d'informations sensibles, comme les noms d'utilisateur et les mots de passe, en utilisant une chaîne encodée en base64. Les programmeurs font cela pour autoriser l'accès à des ressources protégées sur un serveur web.

## Comment faire:

Vous pouvez le faire en Swift en utilisant la classe `URLRequest` et `URLSession`. Voici un exemple:

```Swift
import Foundation

// Création des identifiants
let username = "username"
let password = "password"

// Mise en place de l'URL
let url = URL(string: "https://mywebserver.com")!
var request = URLRequest(url: url)

// Encodage en base64 de vos identifiants
let loginData = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)!
let base64LoginData =  loginData.base64EncodedString()

// Ajout de l'en-tête d'autorisation
request.addValue("Basic \(base64LoginData)", forHTTPHeaderField: "Authorization")

// Envoi de la requête
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else { return }
    print(String(data: data, encoding: .utf8) ?? "")
}
task.resume()
```

## Plongée en profondeur:

L'authentification de base HTTP a été l'une des premières méthodes d'authentification utilisées dans le cadre du protocole HTTP, introduite avec HTTP/1.0 en 1996. Elle est simple, mais elle présente des risques de sécurité, car les informations d'identification ne sont pas fortement cryptées.

Alternativement, des méthodes plus sécurisées comme l'authentification Digest ou OAuth peuvent être utilisées. Toutefois, dans certaines situations où une sécurité élevée n'est pas nécessaire, l'utilisation de l'authentification de base HTTP peut être suffisante.

Lors de l'envoi de la requête, la classe `URLRequest` crée un objet de requête contenant des informations sur les en-têtes HTTP, le corps de la requête et d'autres configurations. `URLSession` envoie la requête en utilisant sa méthode `dataTask`.

## Voir aussi:

- Documentation officielle sur `URLRequest`: https://developer.apple.com/documentation/foundation/urlrequest
- Documentation officielle sur `URLSession`: https://developer.apple.com/documentation/foundation/urlsession
- RFC2617 - Méthodes d'authentification HTTP: https://tools.ietf.org/html/rfc2617