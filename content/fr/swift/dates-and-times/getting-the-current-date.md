---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:56.994228-07:00
description: "Obtenir la date actuelle en Swift implique l'utilisation de la classe\
  \ `Date` pour acc\xE9der \xE0 la date et \xE0 l'heure auxquelles l'application est\
  \ ex\xE9cut\xE9e.\u2026"
lastmod: 2024-02-19 22:05:16.887809
model: gpt-4-0125-preview
summary: "Obtenir la date actuelle en Swift implique l'utilisation de la classe `Date`\
  \ pour acc\xE9der \xE0 la date et \xE0 l'heure auxquelles l'application est ex\xE9\
  cut\xE9e.\u2026"
title: Obtenir la date actuelle
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Obtenir la date actuelle en Swift implique l'utilisation de la classe `Date` pour accéder à la date et à l'heure auxquelles l'application est exécutée. Les programmeurs doivent récupérer la date actuelle pour de nombreuses raisons allant du marquage temporel des événements, à la réalisation de calculs sur les dates, jusqu'à l'affichage des dates et heures dans une interface utilisateur.

## Comment faire :
Le framework `Foundation` de Swift fournit la classe `Date`, rendant simple l'obtention de la date et de l'heure actuelles. Voici un exemple de base de comment obtenir la date actuelle :

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Cela affichera quelque chose comme :

```
2023-04-12 07:46:23 +0000
```

Le format de sortie suit la norme ISO 8601, en utilisant le fuseau horaire UTC. Cependant, vous pourriez vouloir formater cette date à des fins d'affichage. La classe `DateFormatter` de Swift vient à la rescousse :

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

Un exemple de sortie pourrait être :

```
12 avril 2023 à 10:46:23
```

Notez que le format de sortie variera en fonction de la locale du dispositif exécutant le code.

Pour les projets nécessitant des manipulations de dates plus complexes, de nombreux développeurs Swift se tournent vers des bibliothèques tierces telles que `SwiftDate`. Voici comment vous pourriez utiliser `SwiftDate` pour obtenir la date actuelle dans un fuseau horaire et un format spécifiques :

D'abord, ajoutez `SwiftDate` à votre projet en utilisant SPM, CocoaPods, ou Carthage. Ensuite :

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

Cela pourrait afficher :

```
2023-04-12 09:46:23
```

En utilisant `SwiftDate`, vous pouvez facilement manipuler des dates et des heures pour différents fuseaux horaires et locales, simplifiant les tâches de gestion de dates complexes dans vos applications Swift.
