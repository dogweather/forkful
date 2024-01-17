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

## Qu’est-ce que c’est et pourquoi les programmeurs le font?
Envoyer une requête HTTP est un moyen pour les programmeurs d’échanger des données avec des serveurs web. Les programmeurs le font pour récupérer des informations à partir d’un site web, envoyer des données à un serveur, ou tout simplement communiquer avec d’autres applications en ligne.

## Comment faire?
Voici un exemple de code en Swift pour envoyer une requête POST à un serveur en utilisant l’URLSession :
​
```Swift
let url = URL(string: "https://www.monserveur.com/requete")
var request = URLRequest(url: url!)
request.httpMethod = "POST"
let postString = "Données à envoyer au serveur"
request.httpBody = postString.data(using: .utf8)
​
let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Erreur : \(error)")
    } else if let data = data {
        print("Réponse du serveur : \(String(data: data, encoding: .utf8)!)")
    }
}
task.resume()
```

Si tout se passe bien, la console devrait afficher la réponse du serveur en tant que chaîne de caractères.

## En savoir plus
Envoyer des requêtes HTTP est un concept fondamental du développement web qui est apparu dès les débuts d’Internet. Bien qu’il existe d’autres alternatives comme l’utilisation de sockets, la plupart des programmes modernes utilisent des requêtes HTTP pour communiquer avec des serveurs en ligne.

Pour une implémentation plus avancée, il est possible d’utiliser des frameworks comme Alamofire ou AFNetworking pour simplifier le processus et ajouter des fonctionnalités supplémentaires.

Il est également important de connaître les méthodes de requête les plus couramment utilisées, telles que GET, POST, PUT et DELETE, ainsi que les différents codes de réponse HTTP pour pouvoir diagnostiquer les problèmes éventuels.

## À voir aussi
- [Guide officiel de l’URLSession en Swift](https://developer.apple.com/documentation/foundation/urlsession)
- [Documentation d’Alamofire](https://github.com/Alamofire/Alamofire)
- [Documentation d’AFNetworking](https://github.com/AFNetworking/AFNetworking)