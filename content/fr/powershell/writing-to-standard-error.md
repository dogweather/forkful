---
title:                "Écrire dans l'erreur standard"
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire dans la sortie d'erreur standard permet de séparer les messages d'erreur du flux de sortie principal. Les programmeurs font ça pour mieux gérer et diagnostiquer les problèmes.

## How to:
Pour écrire dans la sortie d'erreur standard en PowerShell, utilisez `Write-Error`, `Write-Host -ForegroundColor Red`, ou redirigez le flux avec `2>`.

```PowerShell
# Utilisation de Write-Error
Write-Error "Ceci est un message d'erreur"

# Redirection du flux d'erreur
echo "Ceci est une erreur" 2>&1

# Affichage avec Write-Host
Write-Host "Attention, une erreur s'est produite !" -ForegroundColor Red
```

Les messages s'afficheront en rouge dans la console pour indiquer une erreur.

## Deep Dive
Historiquement, les shells Unix permettaient la redirection des flux standard et d'erreur dès le départ. PowerShell, héritant de cette philosophie, offre des cmdlets pour un contrôle précis. `Write-Error` génère une exception non-terminale, alors que l'opérateur `2>` dirige simplement le flux sans générer d'exception. Pour de l'interception ou du traitement avancé, on peut utiliser `$Error` ou `-ErrorVariable`.

## See Also
- [about_Redirection](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Redirection)
- [Write-Error](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-error)
- [about_Automatic_Variables](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Automatic_Variables)