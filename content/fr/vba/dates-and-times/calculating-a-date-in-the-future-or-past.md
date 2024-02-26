---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:06.976600-07:00
description: "Calculer une date dans le futur ou le pass\xE9 implique de d\xE9terminer\
  \ une date qui se trouve \xE0 un nombre sp\xE9cifi\xE9 de jours, de mois ou d'ann\xE9\
  es d'une date\u2026"
lastmod: '2024-02-25T18:49:54.355918-07:00'
model: gpt-4-0125-preview
summary: "Calculer une date dans le futur ou le pass\xE9 implique de d\xE9terminer\
  \ une date qui se trouve \xE0 un nombre sp\xE9cifi\xE9 de jours, de mois ou d'ann\xE9\
  es d'une date\u2026"
title: "Calculer une date dans le futur ou le pass\xE9"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Calculer une date dans le futur ou le passé implique de déterminer une date qui se trouve à un nombre spécifié de jours, de mois ou d'années d'une date donnée. Les programmeurs ont souvent besoin de cette fonctionnalité pour automatiser des rappels, des abonnements, des dates d'expiration et des tâches de planification dans diverses applications.

## Comment :
Dans Visual Basic pour Applications (VBA), la fonction principale utilisée pour calculer les dates futures ou passées est `DateAdd()`. Cette fonction ajoute un intervalle de temps spécifié à une date, retournant une nouvelle date.

Voici un exemple simple pour ajouter 10 jours à la date actuelle :

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' Ajoute 10 jours à la date actuelle
Debug.Print futureDate ' Affiche quelque chose comme : 20/04/2023
```

De même, pour trouver une date 10 jours dans le passé :

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' Soustrait 10 jours de la date actuelle
Debug.Print pastDate ' Affiche : 31/03/2023, en supposant qu'aujourd'hui soit le 10/04/2023
```

Ces exemples sont assez simples. Vous pouvez remplacer `"d"` par d'autres codes d'intervalle, comme `"m"` pour les mois et `"yyyy"` pour les années, pour effectuer différents types de calculs de dates. Voici comment vous pourriez calculer une date un an dans le futur :

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' Ajoute 1 an à la date actuelle
Debug.Print nextYear ' Affiche : 10/04/2024 si aujourd'hui est le 10/04/2023
```

## Approfondissement
La fonction `DateAdd` fait partie intégrante de VBA depuis sa création, héritant de son prédécesseur BASIC. Bien qu'elle offre une simplicité pour ajouter ou soustraire des intervalles de temps aux dates, il est vital de noter que VBA, y compris ses fonctions de gestion de dates, ne peut pas toujours égaler la commodité ou l'efficacité trouvées dans les langages de programmation plus récents.

Par exemple, des langages modernes comme Python avec le module `datetime` ou JavaScript avec des bibliothèques telles que `moment.js` et `date-fns` offrent des moyens plus intuitifs et puissants pour la manipulation des dates. Ces options fournissent un meilleur support pour la localisation, les fuseaux horaires et les années bissextiles, ce qui peut les rendre plus adaptées pour les applications nécessitant des calculs de dates précis à l'échelle mondiale.

Cependant, pour les macros Excel et les applications nécessitant une intégration au sein de l'écosystème Microsoft Office, VBA reste un choix pratique. La simplicité d'accès direct et de manipulation des données Excel est un avantage significatif. De plus, pour la plupart des calculs de dates basiques comme la planification et les rappels, `DateAdd()` dans VBA offre une solution adéquate et simple. Sa syntaxe est facile à comprendre pour les débutants, tandis que son intégration dans les applications de la suite Office assure sa pertinence dans des cas d'utilisation spécifiques.

En conclusion, alors que les langages de programmation alternatifs peuvent offrir des approches plus modernes au calcul des dates, `DateAdd()` dans VBA sert de témoignage à la puissance de ce langage dans les domaines où il est le plus nécessaire.
