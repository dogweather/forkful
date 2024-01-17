---
title:                "Télécharger une page Web"
html_title:           "Swift: Télécharger une page Web"
simple_title:         "Télécharger une page Web"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Le téléchargement d'une page web consiste simplement à récupérer le contenu d'une page web depuis Internet et à l'afficher sur votre appareil. Les programmeurs le font pour diverses raisons telles que l'analyse de données, la création d'applications mobiles ou pour automatiser des tâches en ligne.

## Comment faire:
````Swift
let url = URL(string: "https://www.example.com")! //Définir l'URL de la page à télécharger
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let data = data { //Si les données sont récupérées avec succès
        if let htmlString = String(data: data, encoding: .utf8) { //Convertir les données en chaîne de caractères
            print(htmlString) //Afficher le contenu de la page web
        }
    }
}
task.resume() //Lancer la tâche de téléchargement
````

## Plongeon en profondeur:
Le téléchargement de pages web est devenu une fonctionnalité courante pour les programmeurs à mesure qu'Internet a pris de l'ampleur. Au fil des ans, différentes méthodes de téléchargement ont été développées, certaines plus efficaces que d'autres. En plus de l'utilisation d'une URL, certaines bibliothèques tierces telles que Alamofire offrent une syntaxe plus simplifiée pour le téléchargement de pages web en Swift.

## À voir également:
Pour en savoir plus sur le téléchargement de pages web en Swift, consultez la documentation officielle d'Apple sur le téléchargement de données brutes via `URLSessionDataTask`. Vous pouvez également découvrir les avantages et les inconvénients des différentes bibliothèques de téléchargement de pages web disponibles pour Swift en lisant des articles de blog ou en consultant des communautés de développeurs en ligne.