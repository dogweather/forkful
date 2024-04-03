---
date: 2024-01-20 15:34:00.673385-07:00
description: "Le parsing HTML, c'est lire et convertir du code HTML en quelque chose\
  \ qu'une application peut comprendre et manipuler. Les programmeurs le font pour\u2026"
lastmod: '2024-03-13T22:44:58.218846-06:00'
model: unknown
summary: Le parsing HTML, c'est lire et convertir du code HTML en quelque chose qu'une
  application peut comprendre et manipuler.
title: Analyse syntaxique de HTML
weight: 43
---

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
