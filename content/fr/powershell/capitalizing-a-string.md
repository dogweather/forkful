---
title:                "Capitaliser une chaîne de caractères"
html_title:           "PowerShell: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi & Comment ?

Capitaliser une chaîne de caractères est le fait de changer la casse des lettres, passant ainsi de lettres minuscules à majuscules ou inversement. Les programmeurs le font pour uniformiser l'apparence des données et faciliter leur manipulation.

# Comment faire :

```powershell
# Définir une chaîne de caractères
$maChaine = "c'est un exemple de chaîne de caractères"

# Utiliser la méthode ToUpper pour la capitaliser
$maChaine.ToUpper()

Output:
C'EST UN EXEMPLE DE CHAÎNE DE CARACTÈRES
```

```powershell
# Définir une chaîne de caractères
$maChaine = "Voici un autre exemple en minuscules"

# Utiliser la méthode ToLower pour la convertir en minuscules
$maChaine.ToLower()

Output:
voici un autre exemple en minuscules
```

# Zoom sur :

## Contexte historique :

La casse des lettres est une notion apparue avec l'utilisation des ordinateurs et des langages de programmation. Avant cela, la distinction entre majuscules et minuscules était principalement utilisée pour des raisons esthétiques ou grammaticales dans la langue écrite.

## Alternatives :

En plus de la méthode ToUpper et ToLower, il est également possible de capitaliser une chaîne de caractères en utilisant un filtre dans PowerShell, tel que ```$maChaine | ForEach-Object { $_.ToUpper() }```. Il existe également des commandes dédiées à cette fonctionnalité, telles que ```ConvertTo-Uppercase``` et ```ConvertTo-Lowercase```.

## Détails de l'implémentation :

La méthode ToUpper et ToLower sont des méthodes de l'objet String dans PowerShell qui utilisent le type [System.String]. Elles retournent une nouvelle instance de String avec la casse modifiée. Ces méthodes ne modifient pas directement la chaîne de caractères originale.

# Voir aussi :

- [Documentation Microsoft sur les méthodes de String](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_string_methods?view=powershell-7)
- [Article Wikipédia sur la casse des lettres](https://fr.wikipedia.org/wiki/Casse_(typographie))