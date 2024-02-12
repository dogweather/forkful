---
title:                "Comparer deux dates"
aliases: - /fr/typescript/comparing-two-dates.md
date:                  2024-01-20T17:34:01.682983-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparer deux dates"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Comparer deux dates, c'est voir laquelle vient avant ou après, ou si elles sont identiques. Les devs le font pour gérer des échéances, trier des événements, etc.

## How to: (Comment faire : )
```TypeScript
const date1: Date = new Date('2023-04-01T00:00:00');
const date2: Date = new Date('2023-04-15T00:00:00');

// Comparer les dates
if(date1 < date2) {
    console.log('date1 est plus tôt que date2');
} else if(date1 > date2) {
    console.log('date1 est plus tard que date2');
} else {
    console.log('Les dates sont identiques');
}

// Affichage des résultats
// "date1 est plus tôt que date2"
```

## Deep Dive (Plongée Profonde)
Historiquement, comparer des dates en JavaScript (et par extension TypeScript) pouvait être délicat à cause des différentes zones horaires et formats. TypeScript, c'est juste JS avec des types, donc les mêmes méthodes s'appliquent. Alternative: bibliothèques comme `moment.js` ou `date-fns` pour plus de contrôle. Détail: TypeScript n'ajoute pas de magie pour les dates, c'est du JavaScript sous les types.

## See Also (Voir Aussi)
- Documentation MDN sur les objets Date: [MDN Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Date-fns, une bibliothèque moderne pour gérer les dates: [date-fns](https://date-fns.org/)
- Moment.js, une autre bibliothèque populaire pour le temps: [moment.js](https://momentjs.com/docs/#/parsing/)
