---
title:                "Mettre en majuscule une chaîne"
aliases:
- /fr/powershell/capitalizing-a-string/
date:                  2024-02-03T19:06:03.889028-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mettre en majuscule une chaîne"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Mettre en majuscule une chaîne dans PowerShell consiste à transformer le premier caractère d'une chaîne donnée en majuscule tout en laissant le reste de la chaîne inchangé. Les programmeurs effectuent souvent cette tâche à des fins de formatage, comme la préparation du texte pour l'affichage dans les interfaces utilisateur ou le suivi des règles grammaticales dans les documents générés.

## Comment faire :
PowerShell, étant un outil polyvalent, vous permet de mettre en majuscule une chaîne en utilisant des méthodes simples sans avoir besoin de bibliothèques tierces. Voici comment vous pouvez le faire :

```powershell
# Utilisant la méthode .Net intégrée 'ToTitleCase' de CultureInfo
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$texteCapitalisé = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $texteCapitalisé
```
Sortie :
```
Hello world
```

Note : Cette méthode met en majuscule la première lettre de chaque mot. Si vous souhaitez strictement mettre en majuscule uniquement la première lettre de la chaîne et laisser le reste tel quel, vous pourriez faire quelque chose comme ceci :

```powershell
# Mettant en majuscule uniquement le premier caractère d'une chaîne
$text = "hello world"
$texteCapitalisé = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $texteCapitalisé
```
Sortie :
```
Hello world
```

PowerShell n'inclut pas directement une fonction simple pour mettre en majuscule uniquement la première lettre d'une chaîne, mais en combinant les méthodes de manipulation de chaîne de base telles que `Substring(0,1).ToUpper()` et la concaténation, nous pouvons facilement atteindre le résultat souhaité.
