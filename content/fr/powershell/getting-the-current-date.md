---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Obtenir la date actuelle signifie récupérer la date et l'heure courantes depuis l'ordinateur local ou un serveur distant. Les programmeurs en ont besoin pour enregistrer le moment précis des événements, comme lorsqu'un utilisateur se connecte à un système.

## Comment faire:

Voici comment obtenir la date actuelle dans PowerShell :

```PowerShell
Get-Date
```

Voici un exemple de résultat : 

```PowerShell
Monday, January 18, 2021 08:20:32 AM
```

Pour un format spécifique, utilisez le paramètre `-Format` ainsi :

```PowerShell
Get-Date -Format "yyyy-MM-dd HH:mm:ss"
```

Exemple de résultat : 

```PowerShell
2021-01-18 08:20:32
```

## En Profondeur

Historiquement, les programmeurs utilisaient des commandes `date` et `time` en batch file ou même en C pour obtenir la date et l'heure. Mais avec PowerShell, tout devient plus simple et plus manipulable.

Comme alternative à `Get-Date`, vous pouvez également utiliser la méthode `Now` de la classe `System.DateTime` :

```PowerShell
[System.DateTime]::Now
```

La commande `Get-Date` est en fait une enveloppe autour de cette méthode, avec plus de flexibilité pour formater les résultats.

## Voir Aussi

- Documentation officielle sur `Get-Date` : https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/get-date 
- Pour une présentation plus complète de la classe `DateTime`, vous pouvez consulter : https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime
- Articles d'introduction à PowerShell : https://learn.microsoft.com/fr-fr/powershell/scripting/overview