---
title:                "Concaténation de chaînes de caractères"
html_title:           "PowerShell: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi?

La concaténation de chaînes de caractères consiste à fusionner plusieurs chaînes pour en créer une seule qui contienne toutes les informations. Les programmeurs utilisent cette technique pour combiner différentes données afin de créer des messages personnalisés, des rapports ou des requêtes de recherche.

## Comment faire:

```PowerShell
# Utilisation de l'opérateur +
"Bonjour" + "le" + "monde"

# Sortie: Bonjourlemonde

# Utilisation de la méthode .Concat()
"Hello ".Concat("world")

# Sortie: Hello world 
```

## Plongeon en profondeur:

La concaténation de chaînes de caractères existe depuis les premiers jours de la programmation informatique et reste aujourd'hui une technique couramment utilisée. Il existe différentes façons de concaténer des chaînes, notamment en utilisant l'opérateur "+" ou la méthode .Concat(). Cependant, il est important de noter que la concaténation de chaînes peut entraîner des problèmes de performance lorsqu'elle est utilisée avec de grandes quantités de données.

## Voir aussi:

- [Documentation Microsoft sur la concaténation de chaînes en PowerShell](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7)
- [Tutorialspoint : Concaténation de chaînes en PowerShell](https://www.tutorialspoint.com/powershell/powershell_concatenation_of_strings.htm)
- [Le guide complet pour les débutants en PowerShell](https://www.lemonway.fr/ressources/Tutoriel_PowerShell.pdf)