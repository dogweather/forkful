---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:54.572526-07:00
description: 'Comment faire : En TypeScript, vous pouvez utiliser l''objet `Date`
  pour obtenir la date et l''heure actuelles. Voici comment vous pouvez le faire .'
lastmod: '2024-03-13T22:44:57.448182-06:00'
model: gpt-4-0125-preview
summary: En TypeScript, vous pouvez utiliser l'objet `Date` pour obtenir la date et
  l'heure actuelles.
title: Obtenir la date actuelle
weight: 29
---

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
