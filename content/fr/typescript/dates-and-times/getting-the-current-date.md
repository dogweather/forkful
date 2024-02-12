---
title:                "Obtenir la date actuelle"
aliases: - /fr/typescript/getting-the-current-date.md
date:                  2024-02-03T19:10:54.572526-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtenir la date actuelle"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Obtenir la date courante en TypeScript, un langage construit sur JavaScript, vous permet d'accéder et de manipuler les informations actuelles de date et d'heure. Les programmeurs ont souvent besoin de cette fonctionnalité pour créer des horodatages, planifier et autres caractéristiques sensibles au temps dans leurs applications.

## Comment faire :
En TypeScript, vous pouvez utiliser l'objet `Date` pour obtenir la date et l'heure actuelles. Voici comment vous pouvez le faire :

```typescript
const currentDate = new Date();
console.log(currentDate);
```

Exemple de sortie :
```
2023-04-12T07:20:50.52Z
```

Ce fragment de code crée un nouvel objet `Date` contenant la date et l'heure actuelles, qui est ensuite imprimé dans la console. Vous pouvez également formater la date en utilisant toLocaleDateString() pour des formats plus lisibles :

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

Exemple de sortie :
```
12/04/2023
```

### Utilisation de date-fns
Pour une manipulation et un formatage de date plus étendus, la bibliothèque `date-fns` est un choix populaire. Tout d'abord, installez-la via npm :

```bash
npm install date-fns
```

Ensuite, vous pouvez l'utiliser pour formater la date courante :

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

Exemple de sortie :
```
2023-04-12
```

Cet exemple `date-fns` formate la date courante en chaîne de caractères au format "AAAA-MM-JJ". La bibliothèque offre une pléthore de fonctions pour la manipulation des dates, la rendant un outil polyvalent pour tout programmeur TypeScript travaillant avec les dates.
