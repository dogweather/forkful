---
title:                "Génération de nombres aléatoires"
html_title:           "PowerShell: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Générer des nombres aléatoires est une fonctionnalité importante pour les programmeurs, car cela leur permet de créer de manière aléatoire des valeurs numériques pour leurs programmes. Cela peut être utile dans des jeux, des simulations, des tests et bien plus encore.

## Comment faire:

Voici comment générer des nombres aléatoires en utilisant PowerShell:

```PowerShell
# Génère un nombre entier aléatoire entre 0 et 100
Get-Random -Minimum 0 -Maximum 100 

# Génère un nombre à virgule aléatoire entre 0 et 1
Get-Random -Minimum 0 -Maximum 1 

# Génère un nombre entier aléatoire entre 1 et 10
Get-Random -Minimum 1 -Maximum 10 

# Génère un nombre aléatoire compris dans une liste spécifiée
$liste = 1, 5, 10, 15, 20
Get-Random -InputObject $liste
```

## Plongée en profondeur:

Générer des nombres aléatoires est une pratique courante en informatique depuis les années 1940. Avant l'apparition des ordinateurs, les programmeurs utilisaient des moyens manuels pour générer des nombres aléatoires, tels que des tables de nombres pré-calculés. De nos jours, il existe d'autres langages de programmation qui proposent également des fonctions de génération de nombres aléatoires, tels que Python et Java. Cependant, PowerShell offre une syntaxe simple et facile à utiliser qui la rend idéale pour les tâches de génération de nombres aléatoires.

## Voir aussi:

- [Documentation officielle de la commande Get-Random](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7)
- [Article sur la génération de nombres aléatoires en Python](https://realpython.com/python-random/)
- [Article sur la génération de nombres aléatoires en Java](https://www.baeldung.com/java-random)