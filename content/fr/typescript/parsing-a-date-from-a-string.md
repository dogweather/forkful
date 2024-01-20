---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La conversion d'une date à partir d'une chaîne de caractères est le processus de passage d'un format texte (par exemple, "2022-02-15") à un objet date utilisable. Les programmeurs le font parce qu'il rend la date compatible avec les opérations spécifiques à cette dernière.

## Comment faire:

Voici un exemple sur comment convertir une chaîne de caractères en date en utilisant TypeScript. 

```TypeScript
let dateChaîne: string = "2022-02-15";
let dateObjet: Date = new Date(dateChaîne);
console.log(dateObjet);
```

Exemple de sortie:

```TypeScript
2022-02-15T00:00:00.000Z
```

## Plongée en profondeur:

Historiquement, JavaScript (et par extension TypeScript) a toujours eu des problèmes avec la gestion des dates à cause des décalages de fuseau horaire. Donc, le but était non seulement de gérer les dates, mais aussi de faciliter le travail avec les fuseaux horaires. 

L'alternative à la conversion manuelle d'une date est d'utiliser une bibliothèque telles que *moment.js*, qui a un support plus complet pour les fuseaux horaires et formats de date. 

Il convient de noter que `new Date()` traite la chaîne de date en différents formats et renvoie la date en heure universelle (UTC). 

## Voir aussi:

Pour plus d'informations sur la manipulation de dates avec TypeScript et JavaScript, consultez les liens ci-dessous:

1. [La documentation officielle de TypeScript](https://www.typescriptlang.org/docs/)
2. [La documentation officielle de JavaScript sur les Dates](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date)
3. [Moment.js, une bibliothèque populaire pour gérer les dates](https://momentjs.com/)