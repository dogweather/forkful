---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## C'est quoi & Pourquoi ?

Convertir une date en chaîne en programmation, c'est transformer une valeur date en une série de caractères. Les programmeurs le font souvent pour faciliter l'affichage ou le stockage des informations de date.

## Comment faire :

Voici un simple exemple d'utilisation de PowerShell pour convertir une date en chaîne.

```PowerShell
$date = Get-Date
$date_string = $date.ToString('dd/MM/yyyy')
```
En exécutant ce code, la date du jour sera convertie en une chaîne de format 'dd/MM/yyyy'.

Exemple de résultat de sortie :

```PowerShell
'23/01/2022'
```

## Plongeon profond :

**(1) Contexte historique** : Depuis sa création, PowerShell permet la conversion des dates en chaînes. Cette fonctionnalité est indispensable compte tenu de la diversité des formats de dates utilisés dans le monde entier.

**(2) Alternatives** : PowerShell n'est pas le seul outil permettant de convertir une date en une chaîne. Les langages de programmation comme Python, Java, C#, entre autres, offrent également cette possibilité.

**(3) Détails de mise en œuvre** : Lors de la conversion d'une date en chaîne dans PowerShell, une surcharge de la méthode ToString() est utilisée. Elle prend un format de date sous forme de chaîne en argument afin de formater le résultat. Chaque élément du format de date (Jour, Mois, Année) est représenté par une lettre ('d' pour le jour, 'M' pour le mois, 'y' pour l'année).

## Voir aussi :

Pour en savoir plus sur la conversion de date en chaîne en PowerShell, consultez les sources suivantes :

- Documentation Microsoft sur la méthode ToString() : [Lien](https://docs.microsoft.com/fr-Fr/dotnet/api/system.datetime.tostring?view=netframework-4.8)
- StackOverflow, un autre excellent exemple de conversion de date en chaîne : [Lien](https://stackoverflow.com/questions/26156213/format-get-date-output-format)