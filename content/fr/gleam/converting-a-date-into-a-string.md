---
title:    "Gleam: Convertir une date en chaîne de caractères."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Pourquoi

La conversion d'une date en chaîne de caractères est un processus courant en programmation. Cela peut être utile pour afficher la date dans un format spécifique, par exemple pour l'affichage dans une interface utilisateur ou dans un fichier de log. Dans cet article, nous allons voir comment réaliser cette conversion en utilisant le langage de programmation Gleam.

# Comment faire

La conversion d'une date en chaîne de caractères peut être réalisée en utilisant la fonction `Formatter.format_date` de la bibliothèque standard de Gleam. Voici un exemple de code :

```Gleam
let date = Time.now()
// Récupère la date actuelle

let formatted_date = Formatter.format_date(date, "%d/%m/%Y")
// Convertit la date en utilisant le format spécifique "%d/%m/%Y" pour obtenir une chaîne de caractères de type "jj/mm/aaaa"

debug_log(formatted_date)
// Affiche la chaîne de caractères dans le fichier de log
```

La sortie de ce code devrait être quelque chose comme `14/06/2021`, en fonction de la date actuelle. Vous pouvez changer le format pour obtenir une représentation différente de la date, en utilisant les lettres de formatage disponibles dans la fonction `format_date` (par exemple `%H` pour l'heure, `%M` pour les minutes, `%Y` pour l'année complète, etc.).

# Plongée en profondeur

La conversion d'une date en chaîne de caractères peut sembler simple, mais il y a en réalité beaucoup de choses qui se passent en arrière-plan. En utilisant le langage Gleam, nous pouvons facilement manipuler les dates grâce à la bibliothèque standard `Time`, qui inclut de nombreuses fonctions utiles telles que `format_date`, `add_days`, `add_hours`, etc. De plus, la fonction `Formatter.format_date` utilise la bibliothèque `DateTimeFormat` qui prend en charge une large gamme de formats de date et d'heures.

# Voir aussi

- [Documentation de la bibliothèque standard de Gleam sur les dates et les heures](https://gleam.run/modules/time.html)
- [Documentation de la bibliothèque standard de Gleam sur les formats de date et d'heure](https://gleam.run/modules/datetimeformat.html)
- [Exemples de manipulation de dates avec Gleam](https://github.com/gleam-lang/examples/tree/main/time)