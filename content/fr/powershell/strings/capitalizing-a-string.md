---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:03.889028-07:00
description: "Comment faire : PowerShell, \xE9tant un outil polyvalent, vous permet\
  \ de mettre en majuscule une cha\xEEne en utilisant des m\xE9thodes simples sans\
  \ avoir besoin\u2026"
lastmod: '2024-03-13T22:44:58.024854-06:00'
model: gpt-4-0125-preview
summary: "PowerShell, \xE9tant un outil polyvalent, vous permet de mettre en majuscule\
  \ une cha\xEEne en utilisant des m\xE9thodes simples sans avoir besoin de biblioth\xE8\
  ques tierces."
title: "Mettre en majuscule une cha\xEEne"
weight: 2
---

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
