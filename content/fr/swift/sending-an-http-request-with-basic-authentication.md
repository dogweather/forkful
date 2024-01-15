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

## Pourquoi

Il est souvent nécessaire d'envoyer des requêtes HTTP avec une authentification de base pour accéder à des ressources protégées sur le web, telles que des API ou des comptes utilisateur. Cela garantit la sécurité des données échangées entre le client et le serveur.

## Comment Faire

Pour envoyer une requête avec une authentification de base en Swift, vous devez suivre les étapes suivantes :

1. Importez le framework `Foundation`
2. Créez une instance de `URL` avec l'URL de la ressource à laquelle vous souhaitez accéder
3. Créez une instance de `URLSession` avec une configuration de base
4. Créez une instance de `URLRequest` en utilisant l'URL précédemment créée
5. Définissez la méthode de requête à `GET` ou toute autre méthode requise
6. Ajoutez un en-tête "Authorization" à votre requête avec les informations d'identification de base encodées en base64
7. Utilisez la méthode `dataTask(with:)` de votre instance de `URLSession` pour envoyer la requête et récupérer les données de réponse
8. Traitez la réponse en utilisant les données et l'objet `URLResponse` renvoyés par la méthode précédente

Voici un exemple de code complet qui envoie une requête GET avec une authentification de base et imprime la réponse:

```Swift 
import Foundation 

// 1. Créer l'URL de la ressource 
let urlString = "https://myapi.com/users"
guard let url = URL(string: urlString) else {
    fatalError("L'URL fournie n'est pas valide")
}

// 2. Créer une instance de URLSession 
let session = URLSession(configuration: .default)

// 3. Créer la requête 
var request = URLRequest(url: url)

// 4. Définir la méthode de requête
request.httpMethod = "GET"

// 5. Ajouter l'en-tête d'authentification
let username = "myusername"
let password = "mypassword"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: .utf8)
if let base64LoginString = loginData?.base64EncodedString() {
    request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
}

// 6. Envoyer la requête et traiter la réponse
let task = session.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Erreur lors de l'envoi de la requête : \(error.localizedDescription)")
        return
    }
    
    guard let data = data, let response = response as? HTTPURLResponse else {
        print("Réponse invalide reçue")
        return
    }
    
    print("Code de réponse : \(response.statusCode)")
    if let dataString = String(data: data, encoding: .utf8) {
        print("Données de réponse : \(dataString)")
    }
}

task.resume() // N'oubliez pas d'appeler resume() pour exécuter la requête
```

## Plongée En Profondeur

Lorsque vous envoyez une requête avec une authentification de base, vous devez encoder les informations d'identification en base64 avant de les inclure dans l'en-tête "Authorization". Cela garantit que les informations restent sécurisées lors de leur transfert sur le réseau.

Il est également important de noter que l'utilisation de l'authentification de base est considérée comme un moyen de sécurité faible, car les informations d'identification peuvent facilement être décodées à l'aide d'un décodeur en ligne. Il est recommandé d'utiliser une authentification plus robuste, telle que l'authentification par jeton, pour protéger vos ressources.

## Voir Aussi

- [Documentation officielle sur URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Guide sur l'authentification de base avec Swift](https://www.raywenderlich.com/710-basic-http-authentication-tutorial-for-ios#toc-anchor-001)