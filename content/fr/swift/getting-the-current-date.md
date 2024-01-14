---
title:    "Swift: Obtenir la date actuelle"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Pourquoi

Aujourd'hui, nous allons parler de la façon d'obtenir la date actuelle en utilisant Swift. La gestion de la date est une partie importante de la programmation, car elle nous permet de suivre le temps et de prendre des décisions basées sur les dates. Que vous ayez besoin de suivre les délais, de calculer des durées ou simplement d'afficher la date à l'utilisateur, il est essentiel de savoir comment obtenir la date actuelle dans votre code.

# Comment Faire

Il existe plusieurs façons d'obtenir la date actuelle en Swift, en fonction de vos besoins. Nous allons en examiner trois ici : en utilisant ```Date()```, en utilisant ```DateFormatter``` et en utilisant ```Calendar```.

## Utilisation de Date()

La façon la plus simple d'obtenir la date actuelle en Swift est d'utiliser la classe intégrée ```Date()```. Il suffit d'appeler cette classe et elle renverra automatiquement la date et l'heure actuelles.

```Swift
let now = Date()
print(now) // Output: 2021-07-25 13:22:47 +0000
```

## Utilisation de DateFormatter

Si vous souhaitez personnaliser le format de la date, vous pouvez utiliser la classe ```DateFormatter```. Vous pouvez spécifier le format souhaité en utilisant des symboles de date prédéfinis ou en créant votre propre format personnalisé.

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy" // Format de date personnalisé
let today = Date()
print(formatter.string(from: today)) // Output: 25/07/2021
```

## Utilisation de Calendar

La dernière méthode que nous allons aborder est l'utilisation de ```Calendar```. Cette classe nous permet de récupérer des informations sur la date actuelle, telles que le jour de la semaine, le mois ou l'année.

```Swift
let calendar = Calendar.current
let dateComponents = calendar.dateComponents([.day, .month, .year], from: Date())
print(dateComponents) // Output: year: 2021 month: 7 day: 25 isLeapMonth: false
```

# Plongée Profonde

Maintenant que nous avons vu les différentes façons d'obtenir la date actuelle en Swift, il est important de comprendre comment cela fonctionne réellement sous le capot. En utilisant la classe ```Date```, Swift utilise en fait une échelle de temps appelée *temps réel*, qui commence le 1er janvier 2001 et s'incrémente en millisecondes. Cependant, pour les calculs basés sur le calendrier, Swift utilise la classe ```Calendar``` et les composants de date tels que l'année, le mois et le jour.

# Voir Aussi

Maintenant que vous savez comment obtenir la date actuelle en Swift, vous pouvez aller plus loin et apprendre comment travailler avec des dates dans votre code. Voici quelques liens utiles :

- [Documentation officielle de Swift sur la gestion du temps](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Didacticiel vidéo sur la gestion de la date en Swift](https://www.youtube.com/watch?v=V6GlC4F4UoI)
- [Article sur les bonnes pratiques de gestion du temps en Swift](https://www.hackingwithswift.com/articles/142/the-ultimate-guide-to-date-formatting-in-swift)