---
title:                "Envoyer une requête http"
html_title:           "Swift: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un développeur en herbe ou un codeur chevronné, il est très probable que vous ayez entendu parler du protocole HTTP. Il s'agit d'un élément crucial dans la construction d'applications Web, car il permet de communiquer avec des serveurs et d'obtenir des données de manière fiable et sécurisée. Dans cet article, nous allons explorer comment envoyer une requête HTTP en utilisant Swift.

## Comment faire

Pour envoyer une requête HTTP avec Swift, nous utiliserons la classe `URLSession` qui fournit une API pour interagir avec des serveurs via les protocoles HTTP et HTTPS. Tout d'abord, nous devons créer une instance de `URLSession` en utilisant la configuration par défaut.

```Swift
let session = URLSession(configuration: .default)
```

Ensuite, nous devons créer une instance de `URLRequest` en spécifiant l'URL de destination, la méthode de la requête et éventuellement des données à envoyer.

```Swift
let url = URL(string: "https://jsonplaceholder.typicode.com/posts")!
var request = URLRequest(url: url)
request.httpMethod = "POST"
let body = "title=Hello&body=World"
request.httpBody = body.data(using: .utf8)
```

Enfin, nous devons utiliser l'instance de `URLSession` pour envoyer notre requête en utilisant la méthode `dataTask`.

```Swift
let task = session.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Erreur : \(error.localizedDescription)")
        return
    }
    if let data = data, let response = response as? HTTPURLResponse {
        if response.statusCode == 200 {
            print(String(data: data, encoding: .utf8) ?? "")
        }
    }
}
task.resume()
```

Dans cet exemple, nous envoyons une requête POST à l'URL `https://jsonplaceholder.typicode.com/posts` avec des données contenant un titre et un corps. En cas de succès, nous affichons le contenu de la réponse.

## Plongée profonde

Maintenant que nous avons vu comment envoyer une requête HTTP avec Swift, il est important de comprendre certains concepts sous-jacents. Tout d'abord, il existe plusieurs méthodes de requête HTTP telles que `GET`, `POST`, `PUT`, `DELETE`, etc. Chacune de ces méthodes a son propre but et il est important de les choisir correctement en fonction de l'action que vous souhaitez effectuer sur le serveur.

Ensuite, il est également important de comprendre les codes de statut de réponse HTTP, tels que `200`, `404`, `500`, etc. Ces codes indiquent si la requête a réussi ou échoué et fournissent des informations supplémentaires sur l'état de la réponse.

Enfin, il faut également prendre en compte les en-têtes de requête et de réponse HTTP. Ces en-têtes fournissent des informations supplémentaires sur la requête ou la réponse, telles que le type de contenu, la longueur du contenu, etc.

# Voir aussi

- [Documentation officielle de URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Liste complète des méthodes HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Methods)
- [Liste des codes de réponse HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Status)