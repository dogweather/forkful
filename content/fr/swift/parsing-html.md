---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:34:00.673385-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Le parsing HTML, c'est lire et convertir du code HTML en quelque chose qu'une application peut comprendre et manipuler. Les programmeurs le font pour extraire des données, manipuler le contenu et intégrer des fonctionnalités web dans leurs applications.

## How to:
Pour parser du HTML en Swift, une librairie populaire est `SwiftSoup`. D'abord, installez-le avec CocoaPods, Carthage ou Swift Package Manager. Voici un exemple simple:

```Swift
import SwiftSoup

let html = "<html><head><title>Salut!</title></head><body><p>C'est du texte dans une page web.</p></body></html>"

do {
    let doc = try SwiftSoup.parse(html)
    let bodyText = try doc.body()?.text()
    print(bodyText!)
} catch Exception.Error(let type, let message) {
    print("Message: \(message)")
} catch {
    print("error")
}
```

Sortie:
```
C'est du texte dans une page web.
```

## Deep Dive:
Historiquement, analyser du HTML était ardu et sujet à erreurs. Les anciennes méthodes utilisaient souvent des expressions régulières, pas vraiment faites pour ça. `SwiftSoup` s'inspire de `Jsoup`, une librairie Java robuste pour le parsing HTML. À l'inverse du parsing avec regex, ces librairies comprennent la structure du HTML, ce qui est plus sécurisé et fiable.

Alternatives : `Kanna`, une autre librairie Swift basée sur `libxml2`, est également utilisée.

Détails d'implémentation : `SwiftSoup` crée un Document Object Model (DOM) facilitant la navigation dans la structure HTML. Vous pouvez sélectionner des éléments spécifiques, effectuer des changements et même nettoyer le code des éléments superflus ou dangereux.

## See Also:
- SwiftSoup sur GitHub: https://github.com/scinfu/SwiftSoup
- Documentation SwiftSoup: http://scinfu.com/SwiftSoup/
- Kanna GitHub : https://github.com/tid-kijyun/Kanna