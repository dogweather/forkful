---
title:                "Travailler avec json"
html_title:           "Swift: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-json.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un développeur iOS et que vous souhaitez récupérer des données à partir du Web, vous avez probablement entendu parler du format JSON. JSON (JavaScript Object Notation) est un format léger et efficace pour échanger des données entre une application et un serveur. Cela peut être utile pour créer des applications qui affichent des données en temps réel, comme des actualités ou des résultats sportifs.

# Comment Faire

La première étape pour travailler avec JSON dans Swift est d'importer le framework Foundation. Ce framework fournit des outils pour travailler avec des données au format JSON.

```Swift
import Foundation
```

Ensuite, vous devez créer un objet de type URL avec l'URL de la source de données JSON. Vous pouvez utiliser la méthode `URL(string:)` pour convertir une chaîne de caractères en objet URL.

```Swift
let urlString = "https://exemple.com/donnees.json"

if let url = URL(string: urlString) {
    // Continuer le code ici
}
```

Une fois que vous avez obtenu un objet URL valide, vous pouvez utiliser la classe `URLSession` pour récupérer les données à partir de l'URL. Cette classe gère les tâches de transfert de données à partir d'URL et peut être personnalisée selon vos besoins.

```Swift
let session = URLSession.shared

let task = session.dataTask(with: url) { (data, response, error) in
    // Gérer les données récupérées ici
}

task.resume()
```

Enfin, vous devez décoder les données JSON en utilisant la classe `JSONDecoder` de Foundation. Cette classe vous permet de convertir des données au format JSON en objets Swift. Vous pouvez spécifier le type d'objet que vous souhaitez obtenir en utilisant le type générique `decode(_:from:)`.

```Swift
let decoder = JSONDecoder()

do {
    // Convertir les données en un objet Article
    let article = try decoder.decode(Article.self, from: data)
    // Utiliser l'objet Article ici
} catch {
    print("Erreur lors de la conversion des données JSON: \(error)")
}
```

# Plongée en Profondeur

Selon la structure de votre JSON, vous devrez créer une structure Swift correspondante pour traiter les données de manière efficace. Vous pouvez utiliser des outils en ligne tels que [JSON to Swift](https://app.quicktype.io/) pour générer automatiquement des structures à partir de votre JSON.

De plus, si vous souhaitez utiliser des clés optionnelles dans votre JSON pour éviter les erreurs potentielles, vous pouvez utiliser le type `Optional` ou le type `Codable`, qui prend en charge la détection de ces clés.

# Voir Aussi

- [Documentation Apple pour JSON avec Swift](https://developer.apple.com/documentation/foundation/jsonserialization)
- [Tutoriel Ray Wenderlich sur JSON en Swift](https://www.raywenderlich.com/1017714-json-tutorial-for-ios-getting-started)