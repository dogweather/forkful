---
title:                "Transformer une date en chaîne de caractères"
html_title:           "Gleam: Transformer une date en chaîne de caractères"
simple_title:         "Transformer une date en chaîne de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez probablement pourquoi vous devriez prendre le temps de convertir une date en chaîne de caractères. Eh bien, cela peut sembler insignifiant, mais cette compétence est extrêmement utile dans la programmation moderne. Que vous souhaitiez afficher une date sur votre site web ou enregistrer une date dans une base de données, savoir comment convertir une date en chaîne de caractères est un élément clé de votre trousse à outils de développement.

## Comment faire

La bonne nouvelle, c'est que Gleam rend cette tâche très simple. Voyons comment procéder en utilisant des exemples de code et une sortie d'exemple.

```
Gleam import time/datetime

let date = time/now()
let formatted_date = datetime/to_string(date)
```

La première ligne importe le module "time/datetime", qui contient toutes les fonctions nécessaires pour travailler avec des dates et des heures. Ensuite, nous utilisons la fonction "now()" du module "time" pour créer un objet de date et l'assigner à la variable "date". Enfin, nous utilisons la fonction "to_string()" du module "datetime" pour convertir notre objet de date en une chaîne de caractères.

La sortie de notre exemple serait quelque chose comme ceci : "2021-04-21 17:30:00".

## Plongée en profondeur

Si vous souhaitez personnaliser le format de votre date, Gleam vous donne également la possibilité de spécifier un format en utilisant la fonction "format()" du module "datetime". Par exemple :

```
Gleam import time/datetime

let date = time/now()
let formatted_date = datetime/format(date, "%m/%d/%Y")
```

Dans cet exemple, nous avons spécifié un format personnalisé "%m/%d/%Y", qui se traduit par "mois/jour/année". Ainsi, la sortie de notre code serait "04/21/2021".

Vous pouvez également utiliser d'autres formats tels que "%H:%M:%S" pour afficher l'heure, ou "%A" pour afficher le jour de la semaine.

## Voir aussi

Si vous souhaitez en savoir plus sur la conversion des dates en chaînes de caractères en utilisant Gleam, consultez ces liens utiles :

- [Documentation officielle de Gleam](https://gleam.run/documentation/)
- [Module "time/datetime" de Gleam](https://gleam.run/modules/time#datetime)
- [Liste complète des formats de date disponibles](https://gleam.run/documentation/stdlib/time/datetime.html#format)