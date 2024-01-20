---
title:                "Supprimer les caractères correspondant à un modèle"
html_title:           "Ruby: Supprimer les caractères correspondant à un modèle"
simple_title:         "Supprimer les caractères correspondant à un modèle"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Supprimer des caractères correspondant à un motif, c'est simplement faire disparaitre les éléments d'une chaîne qui correspondent à un motif spécifique. Les programmeurs le font pour manipuler et nettoyer les données.

## Comment faire :

En PowerShell, il est assez simple de supprimer des caractères d'une chaîne. Voici comment :
```PowerShell
$chaine = "Bonjour le Monde!"
$motif = "o"
$chaine = $chaine -replace $motif,""
```
Ici, tous les "o" de la chaîne seront supprimés. Vous obtiendrez :
```PowerShell
"Bnjur le Mnde!"
```

## Plongée en profondeur :

Historiquement, le PowerShell a été créé pour automatiser les tâches système et pour la gestion de configuration. La suppression des caractères correspondant à un motif est un cas d'utilisation parmi d'autres, familier de la manipulation de chaînes.

En regard des alternatives, dans d'autres langages de scripting comme Python ou Javascript, vous trouveriez des méthodes similaires pour effectuer cette tâche. Cependant, dans PowerShell, la clarté et la concision de la syntaxe font la différence.

Côté mise en œuvre, la suppression des caractères se fait à l'aide de l'opérateur `-replace`. En interne, cet opérateur utilise les expressions régulières du .NET pour correspondre et remplacer les chaînes.

## Voir aussi :

Pour approfondir vos connaissances sur PowerShell et les manipulations de chaînes de caractères, vous pouvez consulter les sources suivantes :

- Tutoriel sur les opérations de chaînes de caractères sous PowerShell : https://ss64.com/ps/syntax-operators.html
- Documentation Microsoft sur l’opérateur `-replace` : https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1#replacement-operator-replace
- Cours de manipulation des chaînes de caractères sur PowerShell : https://www.guru99.com/powershell-string-manipulation.html