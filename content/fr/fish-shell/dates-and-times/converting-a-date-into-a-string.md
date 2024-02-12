---
title:                "Conversion d'une date en chaîne de caractères"
aliases:
- fr/fish-shell/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:11.451936-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une date en chaîne de caractères"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
"Quoi et pourquoi ?" Transformer une date en chaîne de caractères, c'est convertir l'information temporelle en texte. On le fait pour l'afficher, la stocker, ou l'utiliser dans des scripts où le format de date brut ne passe pas.

## How to:
"Comment faire :" Pour convertir une date en chaîne dans Fish, on utilise souvent `date`. Exemple simple :

```Fish Shell
set date_string (date "+%Y-%m-%d")
echo $date_string
```

Sortie :
```
2023-03-15
```

## Deep Dive
"Plongée en profondeur" : Historiquement, `date` vient d'Unix. Fish, moderne et scriptable, facilite la manipulation de dates. Contrairement à d'autres shells, Fish ne requiert pas de mots clefs comme `export` pour définir des variables. Les alternatives incluent l'utilisation de commandes Python ou AWK, mais `date` reste simple et directe. Les formats sont flexibles - `%Y` pour l'année, `%m` pour le mois, `%d` pour le jour, et il y en a bien d'autres.

## See Also
"Voir aussi" :
- Documentation Fish Shell : [link]
- Commande `date` : [link]
- Formatage des dates POSIX : [link]
