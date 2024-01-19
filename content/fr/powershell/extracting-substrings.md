---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Article PowerShell : Extraire des Sous-Chaînes

## Quoi et Pourquoi?

Extraire des sous-chaînes, c'est retirer une portion spécifique d'une chaîne de caractères. Les programmeurs le font pour manipuler ou analyser des informations spécifiques à partir d'un texte plus large.

## Comment faire:

PowerShell utilise plusieurs méthodes pour extraire des sous-chaînes. Voyons cela avec des exemples:

```PowerShell
$s = "Hello, PowerShell"
$s.Substring(0, 5) 
```
Ceci retournera "Hello" car nous avons démarré à l'index 0 et pris 5 caractères.

```PowerShell
$s.Substring(7)
```
Ici, on obtiendra "PowerShell" car nous avons démarré à l'index 7 jusqu'à la fin de la chaîne.

## Plongée Profonde

L'extraction de sous-chaînes est un élément fondamental dans la plupart des langages de programmation depuis leur apparition. En Powershell, nous avons aussi une autre approche qui utilise le découpage de chaîne basé sur un caractère spécifique avec "-split". Par exemple:

```PowerShell
$s = "Bonjour, PowerShell"
$s -split "," 
```
La sortie sera un tableau avec "Bonjour" et "PowerShell".

Cependant, la méthode `Substring` reste la plus intuitive et la plus directe si nous voulons extraire une certaine portion de la chaîne en se basant sur les index.

## Voir Aussi

Pour aller plus loin, consultez ces ressources:

- [Documentation officielle de Microsoft pour la méthode Substring](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.substring?view=net-5.0)
- [Documentation officielle de Microsoft pour l'opérateur -split](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.core/about/about_split?view=powershell-7.1)