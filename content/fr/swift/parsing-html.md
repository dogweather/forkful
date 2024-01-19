---
title:                "Analyser le HTML"
html_title:           "Kotlin: Analyser le HTML"
simple_title:         "Analyser le HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'analyse HTML (HTML parsing) est le processus d'interprétation du code HTML pour le transformer en une structure de données manipulable. On l'utilise pour extraire des données spécifiques depuis un document HTML, nécessaire pour le développement web et le web scraping.

## Comment faire :

Tout d'abord, installez le package `SwiftSoup` via Swift Package Manager.

```Swift
 .package(url: "https://github.com/scinfu/SwiftSoup.git", from: "2.3.2")
```

Pour analyser du HTML, utilisez le code suivant:

```Swift
import SwiftSoup

let html = "<html><body><p>Hello, World!</p></body></html>"
do {
    let doc: Document = try SwiftSoup.parse(html)
    let bodyText: String = try doc.body()!.text()
    print(bodyText)
}
catch Exception.Error(_, let message) {
    print(message)
}
catch {
    print("error")
}
```

Dans ce code, "Hello, World!" sera imprimé à la console.

## Plongée profonde 

L'analyse HTML a une longue histoire; bien avant Swift, les langages tels que Perl, Python et PHP étaient utilisés pour cela. Swift est de plus en plus préféré pour son efficacité et sa sécurité. 

Il existe de nombreuses alternatives à SwiftSoup pour analyser du HTML en Swift, y compris Kanna et Ji. Chacun a ses propres avantages et inconvénients, donc votre choix dépendra de vos besoins spécifiques. 

Lors de l'analyse HTML avec SwiftSoup, le HTML est converti en une structure connue sous le nom d'arbre DOM (Document Object Model). C'est une représentation en arbre des éléments HTML du document qui permet le traitement et la manipulation.

## Voir aussi 

1. Documentation SwiftSoup: https://github.com/scinfu/SwiftSoup
2. Kanna, une autre librairie Swift pour l'analyse HTML: https://github.com/tid-kijyun/Kanna
3. Tutoriel sur l'analyse HTML en Swift: https://www.hackingwithswift.com/articles/140/how-to-make-sense-of-html-using-swiftsoup