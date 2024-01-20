---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi ?

Trouver la longueur d'une chaîne signifie déterminer combien de caractères elle contient. Les programmeurs le font pour manipuler et interagir avec les données de manière plus efficace.

# Comment faire :

Pour obtenir la longueur d'une chaîne en PowerShell, utilisez la propriété `.Length`. Voici un exemple:

```PowerShell
$str = "Bonjour le monde"
$length = $str.Length
echo $length
```

L'exécution de ces lignes vous donnera `16`, qui est la longueur de la chaîne `"Bonjour le monde"`.

# Approfondissement 

Historiquement, les fonctions de traitement des chaînes font partie intégrante de la programmation depuis sa création. Le concept est basique, mais essentiel dans toutes les formes de programmation. 

En PowerShell, l'alternative à `.Length` serait d'utiliser la cmdlet `Measure-Object`. Par exemple:

```PowerShell
$str = "Bonjour le monde"
$length = ($str | Measure-Object -Character).Characters
echo $length
```

Cependant, `.Length` est plus rapide et plus efficace que `Measure-Object`, c'est donc généralement le choix préféré.

Sur le plan de l'implémentation, `.Length` fait partie du framework .NET et est hérité par chaque chaîne dans PowerShell. Il compte les caractères Unicode, y compris les espaces, les ponctuations et les caractères spéciaux.