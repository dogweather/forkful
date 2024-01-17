---
title:                "Trouver la longueur d'une chaîne"
html_title:           "PowerShell: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Trouver la longueur d'une chaîne de caractères est un processus qui permet de déterminer le nombre de caractères composant une chaîne. Les programmeurs effectuent cette opération pour diverses raisons, notamment pour valider les entrées de l'utilisateur, traiter des données et formater des chaînes de manière dynamique.

## Comment faire :
Voici deux façons courantes de trouver la longueur d'une chaîne de caractères en PowerShell :

```PowerShell
$chaine = "Bonjour le monde !" #Définition d'une variable contenant une chaîne de caractères
$chaine.Length #Utilisation de la propriété Length pour obtenir la longueur de la chaîne
```
Résultat : 18

```PowerShell
$chaine = Read-Host "Entrez une chaîne de caractères" #Invitation à l'utilisateur d'entrer une chaîne
$chaine.Length #Utilisation de la propriété Length pour obtenir la longueur de la chaîne entrée par l'utilisateur
```
Résultat : la longueur de la chaîne entrée par l'utilisateur

## Plongez en profondeur :
La recherche de la longueur d'une chaîne de caractères a été une tâche courante pour les programmeurs depuis les débuts de la programmation. De nos jours, il existe de nombreuses façons d'effectuer cette opération, comme l'utilisation de fonctions spécifiques ou de méthodes intégrées. Cependant, en PowerShell, la propriété Length reste l'option la plus simple et la plus efficace pour trouver la longueur d'une chaîne de caractères.

## Voir aussi :
Pour en savoir plus sur la propriété Length et d'autres façons de trouver la longueur d'une chaîne de caractères en PowerShell, consultez les sources suivantes :
- [Documentation officielle de Microsoft sur la propriété Length](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.length?view=net-5.0)
- [Article sur la recherche de la longueur d'une chaîne en PowerShell par Microsoft Scripting Guy](https://devblogs.microsoft.com/scripting/powertip-use-powershell-to-find-string-length)