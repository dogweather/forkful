---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "PowerShell: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Supprimer des caractères correspondant à un modèle est une opération courante dans la programmation où l'on souhaite supprimer certaines parties d'une chaîne de caractères selon des critères spécifiés. Cela peut être utile lors du traitement de données ou pour formater correctement une chaîne avant de l'afficher.

## Comment faire:

Voici quelques exemples de code en PowerShell pour supprimer des caractères correspondant à un modèle et le résultat correspondant:

```PowerShell
# Supprimer tous les nombres d'une chaîne de caractères
$string = "Hello123World"
$string -replace "[0-9]" 

# Résultat: Helloworld

# Supprimer les caractères spéciaux d'une chaîne
$string = "Hello$#@World"
$string -replace "[^a-zA-Z0-9]"

# Résultat: Helloworld
```

## Plongée en profondeur:

### Contexte historique:
Le remplacement de caractères correspondant à un modèle existe depuis longtemps dans de nombreux langages de programmation populaires tels que Perl, Awk ou Unix. En PowerShell, cela peut être fait à l'aide de l'opérateur de remplacement de chaîne `-replace` qui accepte un motif (pattern) en entrée.

### Alternatives:
Il existe d'autres moyens de supprimer des caractères correspondant à un modèle en utilisant des méthodes telles que `Trim()` ou `Substring()` en combinant avec des expressions régulières. Cependant, l'utilisation de l'opérateur `-replace` est généralement plus simple et plus efficace.

### Détails de mise en œuvre:
L'opérateur `-replace` utilise les expressions régulières pour trouver et remplacer des parties d'une chaîne de caractères selon un motif spécifique. Les expressions régulières sont des séquences de caractères qui permettent de définir des motifs complexes à rechercher dans une chaîne.

## Voir aussi:

- [Microsoft Docs - Opérateur -replace](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7#replace)
- [Tutorialspoint - PowerShell -replace](https://www.tutorialspoint.com/PowerShell/PowerShell_replace_operators.htm)
- [Rédiger des expressions régulières en PowerShell](https://blog.netwrix.com/2018/09/12/how-to-create-regular-expressions-for-powershell/)