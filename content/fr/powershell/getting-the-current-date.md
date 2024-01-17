---
title:                "Obtenir la date actuelle"
html_title:           "PowerShell: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Obtenir la date actuelle est une tâche courante dans la programmation. Cela consiste simplement à récupérer la date et l'heure actuelles sur votre ordinateur. Les programmeurs le font pour diverses raisons, telles que l'enregistrement d'une date de création ou de modification d'un fichier, ou pour exécuter des tâches planifiées en fonction d'une heure spécifique.

## Comment faire:

Voici quelques exemples de code en PowerShell pour obtenir la date actuelle:

```powershell
# Obtenir la date actuelle dans le format par défaut de votre système
Get-Date

# Obtenir la date actuelle dans un format spécifique (exemple: jour/mois/année)
(Get-Date).ToString("dd/MM/yyyy")

# Obtenir la date actuelle dans un fuseau horaire spécifique (exemple: heure de Paris)
Get-Date -Format "dd/MM/yyyy HH:mm:ss" -TimeZone "Romance Standard Time"
```

Voici un aperçu de la sortie pour chacun de ces exemples:

```
09 Septembre 2021 13:00:00

09/09/2021

09/09/2021 14:00:00
```

## Plongée en profondeur:

Pour comprendre un peu plus le concept d'obtention de la date actuelle, il est utile d'examiner son contexte historique. Dans les anciennes versions de Windows, il était courant d'utiliser la commande "date /t" dans l'invite de commandes pour afficher la date actuelle. Cependant, avec l'évolution des systèmes d'exploitation, il est maintenant préférable d'utiliser la commande Get-Date en PowerShell pour obtenir la date actuelle.

Il existe également d'autres façons d'obtenir la date actuelle dans PowerShell, telles que l'utilisation de la commande Get-TimeZone pour afficher la date et l'heure dans un fuseau horaire spécifique. Il est également possible d'utiliser une fonction d'horodatage en PowerShell pour afficher la date et l'heure dans un format personnalisé.

L'implémentation de la commande Get-Date en PowerShell est basique et ne nécessite pas de paramètres supplémentaires. Cependant, pour des besoins plus avancés, il est possible d'utiliser différents paramètres pour obtenir la date sous forme de chaîne de caractères ou dans un fuseau horaire spécifique.

## Voir aussi:

Pour en savoir plus sur l'utilisation de la commande Get-Date en PowerShell, voici quelques liens utiles:

- [Documentation Microsoft sur la commande Get-Date](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/get-date)
- [Exemples de scripts PowerShell pour travailler avec la date et l'heure](https://www.jesusninoc.com/07/06/ejemplos-de-scripts-1-6-powershell-work-with-dates-and-times-languageharness/)
- [Vidéo YouTube expliquant l'utilisation de la commande Get-Date en détail](https://www.youtube.com/watch?v=6EcKlw_pK8o)