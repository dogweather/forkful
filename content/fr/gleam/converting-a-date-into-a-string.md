---
title:    "Gleam: Transformer une date en chaîne de caractères"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Pourquoi

Convertir une date en chaîne de caractères peut sembler être une tâche assez simple, mais cela peut s'avérer très utile dans certaines situations. Par exemple, cela peut être nécessaire lors de la manipulation de données pour les faire correspondre à un certain format ou lors de l'affichage d'informations dans un format spécifique.

##Comment faire

Pour convertir une date en chaîne de caractères en utilisant Gleam, vous pouvez utiliser la fonction `LocalDateTime.to_string()`. Voici un exemple de code avec une date précise et le format de chaîne de caractères voulu :

```Gleam
import gleam/datetime.{ LocalDateTime }

let date = LocalDateTime.from_date(2021, 8, 16)
let formatted_date = date.to_string("%Y-%m-%d")

IO.println(formatted_date) // output: 2021-08-16
```

Comme vous pouvez le voir dans l'exemple, la fonction `to_string()` prend un argument de format facultatif, qui vous permet de personnaliser la façon dont la date est affichée en tant que chaîne de caractères. Vous pouvez utiliser des codes de formatage spécifiques pour changer l'ordre, le format ou l'affichage des éléments de la date.

## Plongée en profondeur

La raison pour laquelle `LocalDateTime.to_string()` fonctionne est parce qu'elle utilise le module `gleam/datetime/format` qui définit des fonctions pour convertir différents types de données en chaînes de caractères selon différents formats. Le module utilise également la bibliothèque d'extension Erlang `calendar` pour effectuer les conversions.

Pour une liste complète des codes de formatage disponibles, vous pouvez consulter la documentation d'Erlang pour le module `calendar`.

## Voir aussi

- [Documentation Gleam pour LocalDateTime](https://gleam.run/modules/gleam/datetime/#LocalDateTime)
- [Documentation Erlang pour le module calendar](https://erlang.org/doc/man/calendar.html)