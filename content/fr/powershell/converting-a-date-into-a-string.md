---
title:                "Conversion d'une date en chaîne de caractères"
html_title:           "PowerShell: Conversion d'une date en chaîne de caractères"
simple_title:         "Conversion d'une date en chaîne de caractères"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pour quoi faire?
Connaître comment convertir une date en une chaîne de caractères est une compétence importante pour les programmeurs PowerShell. Cela signifie essentiellement prendre une date au format d'objet et la transformer en une présentation textuelle, par exemple 01/01/2021 devient "1er janvier 2021". Cette conversion est utile lors de l'affichage de dates dans des rapports ou lors de la manipulation de données dans certains formats de date spécifiques.

## Comment faire:
```PowerShell
# Exemple de conversion d'une date en une chaîne de caractères
$aujourdhui = Get-Date
$aujourdhui.ToShortDateString()

# Sortie: 12/08/2021

# Exemple avec un format spécifique
$aujourdhui.ToString("dddd, MMMM dd yyyy")

# Sortie: jeudi, août 12 2021

# Vous pouvez également combiner des chaînes de caractères et des dates pour obtenir un résultat personnalisé
$nom = "John"
$naissance = Get-Date -Year 1990 -Month 03 -Day 15
"Bonjour $nom, votre date de naissance est $($naissance.ToShortDateString())!"

# Sortie: Bonjour John, votre date de naissance est 15/03/1990!
```
Pour plus d'informations sur les formats de date et les options de conversion en chaîne de caractères, consultez la [documentation officielle de Microsoft sur Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1) ou utilisez la commande `Get-Help Get-Date` dans votre console.

## Approfondissement:
Bien que la conversion d'une date en une chaîne de caractères soit courante dans la programmation, il est important de comprendre les contextes historiques et les alternatives disponibles. Avant l'utilisation de Get-Date, la commande [get-psprovider](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-psprovider?view=powershell-7.1) était couramment utilisée, mais elle est maintenant considérée comme obsolète.

De plus, il existe des modules tiers tels que [DateUtils](https://www.powershellgallery.com/packages/DateUtils/) qui offrent des fonctionnalités supplémentaires pour la manipulation et la conversion de dates en PowerShell.

## Voir aussi:
- [Get-Date documentation officielle de Microsoft](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [DateUtils module tiers pour PowerShell](https://www.powershellgallery.com/packages/DateUtils/)
- [Conseils pour manipuler les dates en PowerShell](https://www.pdq.com/blog/working-with-dates-in-powershell/)