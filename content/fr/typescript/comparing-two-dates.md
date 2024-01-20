---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Comparer deux dates c'est une action commune en programmation pour déterminer laquelle est la plus récente ou combien de temps s'est écoulé entre elles. C'est essentiel pour la gestion des tâches, le filtrage des données et la mise en place de délais.

## Comment faire:

Nous pouvons comparer deux dates en utilisant les méthodes intégrées de l'objet `Date` en TypeScript.

```TypeScript
let date1 = new Date(2021, 10, 1);
let date2 = new Date(2022, 10, 1);

// Comparer les dates
if(date1 > date2){
   console.log("date1 est plus récente");
}
else {
   console.log("date2 est plus récente");
}

// Sortie: date2 est plus récente
```

## Plongée en profondeur:

Historiquement, JavaScript (et par extension, TypeScript) offre plusieurs méthodes pour manipuler et comparer les dates. Bien qu'il existe des bibliothèques tierces comme moment.js offrant davantage de fonctionnalités, la méthode ci-dessus reste simple et efficace. L'operator `>` convertit implicitement les objets de date en millisecondes depuis l'époque UNIX (01-01-1970), facilitant ainsi la comparaison.

Au cas où la tâche de comparaison devient plus complexe, par exemple, considérer uniquement les jours et ignorer l'heure, nous pourrions remettre à zéro l'heure sur nos dates avant la comparaison.

```Typescript
date1.setHours(0,0,0,0);
date2.setHours(0,0,0,0);
```

Alternativement, nous pouvons convertir les dates en chaînes au format 'AAAA-MM-JJ' pour une comparaison basée sur les chaînes.

## Voir aussi:

- Documentation Mozilla pour l'objet Date: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date
- Comparaison de date dans JavaScript : https://stackoverflow.com/questions/492994/compare-dates-with-javascript
- Comparer des dates en ignorant l'heure: https://stackoverflow.com/questions/2698725/comparing-date-part-only-without-comparing-time-in-javascript