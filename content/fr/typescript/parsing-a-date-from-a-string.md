---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:38:36.399982-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Transformer une date sous forme de chaîne de caractères en objet date est essentiel pour manipuler des dates. Les programmeurs le font pour trier, comparer, et opérer des calculs sur des données temporelles.

## Comment faire :
```typescript
// Utilisation de l'objet Date intégré
let dateString: string = '2023-04-12T15:20:30Z';
let dateObject: Date = new Date(dateString);
console.log(dateObject);

// Utilisation de bibliothèques tierces comme date-fns
import { parseISO } from 'date-fns';
let dateFromString = parseISO(dateString);
console.log(dateFromString);
```
Output:
```
2023-04-12T15:20:30.000Z
2023-04-12T15:20:30.000Z
```

## Plongée en profondeur
Historiquement, les JavaScripteurs utilisaient l'objet `Date` natif pour manipuler les dates. Cependant, la complexité des dates et des fuseaux horaires a conduit à la création de bibliothèques spécialisées comme `Moment.js` ou `date-fns`. Ces outils offrent des fonctions de parsing plus robustes et flexibles. 

Dans TypeScript, le type de date est simplement `Date`, et le parsing est identique à JavaScript, mais on bénéficie de la vérification de type en amont. Attention aux formats de date ! La norme ISO 8601 (`YYYY-MM-DDTHH:mm:ss.sssZ`) est recommandée pour éviter les confusions. 

## Voir aussi
- Documentation de `date-fns` : https://date-fns.org/v2.28.0/docs/parseISO
- Guide de l'objet `Date` : https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js pour les nostalgiques : https://momentjs.com/docs/#/parsing/
