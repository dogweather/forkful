---
date: 2024-01-20 17:35:27.607852-07:00
description: "How to: (Comment faire :) Voil\xE0 quelques fa\xE7ons de concat\xE9\
  ner des cha\xEEnes dans PowerShell ."
lastmod: '2024-04-05T21:53:59.489681-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) Voil\xE0 quelques fa\xE7ons de concat\xE9ner des cha\xEE\
  nes dans PowerShell ."
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

## How to: (Comment faire :)
Voilà quelques façons de concaténer des chaînes dans PowerShell :

```PowerShell
# Utilisation de l'opérateur +
$greeting = "Salut, " + "comment ça va ?"
$greeting

# Avec la substitution de variables
$who = "le monde"
$greeting = "Bonjour, $who!"
$greeting

# Via l'opérateur -f (format)
$name = "Jean"
$greeting = "Coucou, {0}!" -f $name
$greeting

# Ou joindre un array avec le -join
$words = "Ceci", "est", "une", "phrase."
$sentence = $words -join " "
$sentence
```

Résultats :
```
Salut, comment ça va ?
Bonjour, le monde!
Coucou, Jean!
Ceci est une phrase.
```

## Deep Dive (Plongée en profondeur)
Historiquement, la concaténation de chaînes remonte aux débuts de la programmation : c'est une opération fondamentale. En PowerShell, elle est facile et flexible. Au-delà de l'opérateur `+` basique, la substitution de variables et l'opérateur `-f` permettent une mise en forme plus fine. Pourquoi s'en soucier ? 

L'opérateur `+` est intuitif mais peut être lent si on l'utilise pour concaténer beaucoup de chaînes dû à l'immuabilité des strings en .NET (chaque concaténation crée une nouvelle string). L'opérateur `-f` est pratique pour un formatage complexe, alors que `-join` est le roi de la performance quand il faut assembler des éléments d'un array.

## See Also (Voir également)
- [About Join](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_join?view=powershell-7.1)
- [About Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Operators?view=powershell-7.1)
- [PowerShell String Formatting](https://kevinmarquette.github.io/2017-01-13-powershell-variable-substitution-in-strings/)
