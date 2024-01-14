---
title:                "TypeScript: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Saviez-vous que la date et l'heure actuelles étaient essentielles pour de nombreuses applications ? Que ce soit pour afficher une date de publication d'un article, enregistrer un événement dans un calendrier ou calculer le temps écoulé depuis une certaine date, la gestion du temps est omniprésente dans le monde de la programmation. Dans cet article, nous allons vous expliquer comment obtenir la date actuelle en utilisant TypeScript.

## Comment faire

Tout d'abord, vous devez importer la classe `Date` de TypeScript en utilisant `import { Date } from 'typescript'`. Cela vous permettra d'utiliser les méthodes et propriétés de la classe dans votre code.

Ensuite, vous pouvez simplement utiliser la méthode `now()` pour obtenir la date et l'heure actuelles. L'objet renvoyé par cette méthode contiendra des informations telles que l'année, le mois, le jour, l'heure, la minute et la seconde. Voici un exemple de code:

```TypeScript
import { Date } from 'typescript';

let dateActuelle = Date.now();

console.log(dateActuelle);
// Output : 1633454032831 (timestamp en millisecondes)
```

Vous pouvez également utiliser la méthode `toLocaleDateString()` pour obtenir la date sous forme de chaîne de caractères dans un format spécifique. Cette méthode prend en paramètre une chaîne de langue et des options de formatage. Voici un exemple de code:

```TypeScript
import { Date } from 'typescript';

let dateActuelle = new Date();

console.log(dateActuelle.toLocaleDateString('fr-FR', { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' }));
// Output : mercredi 6 octobre 2021
```

## Plongée en profondeur

Maintenant que vous savez comment obtenir la date actuelle en utilisant TypeScript, vous pouvez également explorer davantage la classe `Date` et ses méthodes. Par exemple, la méthode `getDay()` vous permet de récupérer le jour de la semaine (en utilisant des nombres de 0 à 6, 0 étant dimanche).

De plus, la classe `Date` prend également en charge la gestion des fuseaux horaires et des dates nécessitant une précision jusqu'à la milliseconde.

## Voir aussi 

- [La documentation officielle de TypeScript sur la classe Date](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-6.html#improved-accuracy-of-es2015-date-object)
- [Un guide complet sur la gestion du temps en JavaScript et TypeScript](https://www.digitalocean.com/community/tutorials/js-js-date-and-date-time-objects-in-javascript)

Merci d'avoir lu cet article sur la récupération de la date actuelle en TypeScript. N'hésitez pas à explorer davantage la classe Date pour découvrir toutes ses fonctionnalités. À bientôt pour un prochain article sur la programmation en TypeScript !