---
title:    "Gleam: Obtenir la date actuelle"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi

Saviez-vous qu'il est possible d'obtenir automatiquement la date actuelle dans vos programmes Gleam ? Cela peut sembler une petite fonctionnalité, mais elle peut être très utile pour de nombreuses raisons. Dans cet article, nous allons vous expliquer pourquoi vous devriez utiliser cette fonctionnalité et comment le faire de manière efficace.

## Comment faire

Pour obtenir la date actuelle en utilisant Gleam, nous allons utiliser la fonction `Date.now()` du module `Time` :

```Gleam
import Time

let current_date = Time.Date.now()
```

Cette fonction renvoie un `Time.Date` avec les informations sur la date et l'heure actuelles. Voici à quoi ressemblera le résultat :

```Gleam
{ year: 2020, month: July, month_day: 8, hour: 10, minute: 30, second: 59 }
```

Vous pouvez également modifier le fuseau horaire en utilisant le paramètre `utc_offset`, comme ceci :

```Gleam
let current_date = Time.Date.now(utc_offset = 3)
```

## Plongée en profondeur

La fonction `Date.now()` utilise le temps UNIX pour déterminer la date et l'heure actuelles. Cela signifie que le résultat sera différent selon le fuseau horaire de votre ordinateur, mais il sera toujours calculé en utilisant le temps UNIX, qui est le nombre de secondes écoulées depuis le 1er janvier 1970 à minuit UTC.

De plus, vous pouvez également utiliser la fonction `Date.from_timestamp(timestamp)` pour créer une date à partir d'un timestamp UNIX spécifique. Cela peut être utile si vous devez convertir une date de votre système en utilisant Gleam.

# Voir aussi

- La documentation officielle de Gleam sur les dates et le temps : https://gleam.run/documentation/stdlib/time/
- Un tutoriel sur les bases de la programmation avec Gleam : https://gleam.run/getting-started/
- Un article sur la gestion des erreurs en utilisant le type `Result` en Gleam : https://gleam.run/documentation/the-basics/errors/