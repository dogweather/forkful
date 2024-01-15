---
title:                "Obtenir la date actuelle"
html_title:           "Gleam: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà eu besoin de connaître la date actuelle dans votre code, vous savez que cela peut être une tâche fastidieuse et compliquée. Heureusement, avec Gleam, il existe une manière simple et efficace de récupérer la date courante.

## Comment faire

Pour obtenir la date courante en Gleam, utilisez simplement la fonction `Date.current()`, qui retournera un enregistrement contenant les informations sur la date actuelle. Voici un exemple de code:

```
Gleam import Date

let date = Date.current()

IO.print(ln: "La date actuelle est: #{date.year}-#{date.month}-#{date.day}")
```

Et voici ce que vous pouvez attendre en terme de sortie: 

```
La date actuelle est: 2021-09-15
```

## Plongée approfondie

La fonction `Date.current()` utilise le fuseau horaire actuel de votre système pour déterminer la date. Cela signifie que la date peut être différente en fonction de l'emplacement de votre serveur. Si vous souhaitez spécifier un fuseau horaire différent, vous pouvez utiliser la fonction `Date.current(Timezone.(nom_du_fuseau))`. Vous pouvez également utiliser cette fonction pour obtenir la date et l'heure, en ajoutant des informations supplémentaires à l'enregistrement retourné par la fonction `Date.current()`.

## Voir aussi

- [Documentation officielle de Gleam sur les dates](https://gleam.run/documentation/standard_library/dates.html)
- [Tutoriel sur les dates avec Gleam](https://dev.to/gleamlang/getting-started-with-gleam-dates-57m)