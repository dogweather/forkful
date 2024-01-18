---
title:                "Analyser une date à partir d'une chaîne de caractères."
html_title:           "PowerShell: Analyser une date à partir d'une chaîne de caractères."
simple_title:         "Analyser une date à partir d'une chaîne de caractères."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Alors, tu veux apprendre à extraire une date d'une chaîne de caractères en PowerShell? Pas de problème! C'est en fait une tâche assez courante pour les programmeurs, surtout lorsqu'ils travaillent avec des données provenant de différentes sources. Dans cet article, nous allons plonger directement dans le vif du sujet et te montrer comment le faire de manière efficace et simple. Pas de temps à perdre, commençons!

## Quoi et Pourquoi?

Parsing une date à partir d'une chaîne de caractères en PowerShell signifie en gros extraire la date dans un format spécifique à partir d'une chaîne de texte plus longue. Pourquoi le faut-il? Eh bien, cela est nécessaire lorsque tu travailles avec des données provenant de différentes sources, comme des fichiers CSV ou des bases de données, et que tu as besoin de les formater correctement pour les manipuler plus facilement. En bref, cela te permet d'avoir un contrôle total sur la façon dont une date est présentée.

## Comment faire?

La façon la plus simple de parser une date à partir d'une chaîne en PowerShell est d'utiliser la cmdlet `Get-Date`. Celui-ci accepte une chaîne de caractères en entrée, ainsi qu'un formatage personnalisé, pour extraire la date désirée. Voici un exemple:

```PowerShell
Get-Date "20 juin 2021" -Format "dd/MM/yyyy"
```

Cela produira une sortie de `20/06/2021`. Comme tu peux le voir, tu n'as même pas besoin de spécifier le format de base, car `Get-Date` peut le déterminer automatiquement. Toutefois, si tu veux une sortie plus précise, tu peux ajouter le paramètre `-UFormat` suivi d'un formatage spécifique. Par exemple:

```PowerShell
Get-Date "20 juin 2021" -UFormat "%A, %d %B %Y"
```

Cela donnera une sortie de `dimanche, 20 juin 2021`.

## Plongée en profondeur

Maintenant que tu sais comment parser une date, peut-être aimerais-tu en savoir plus sur son historique. Avant l'arrivée des ordinateurs, les dates étaient généralement écrites de différentes manières en fonction de la culture et du lieu. Cela a conduit à des problèmes de compréhension et de communication entre les différentes parties du monde. C'est pourquoi un formatage standard a été développé, connu sous le nom de "ISO 8601", qui spécifie précisément comment les dates doivent être écrites. PowerShell utilise ce formatage par défaut, mais tu peux toujours personnaliser en fonction de tes besoins.

Si tu préfères une alternative à `Get-Date`, tu peux également utiliser la cmdlet `ParseExact`, qui te permet de spécifier directement le format de date souhaité en utilisant le paramètre `-ExactFormat`. L'utilisation de cette commande peut être un peu plus complexe, mais elle offre une plus grande flexibilité.

## À voir également

Pour plus d'informations sur la manipulation de dates en PowerShell, tu peux consulter la documentation officielle de Microsoft sur `Get-Date` et `ParseExact`. Tu peux également consulter des tutoriels et des forums en ligne pour des exemples pratiques et des astuces supplémentaires. Bon codage!