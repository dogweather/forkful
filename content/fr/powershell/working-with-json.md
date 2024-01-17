---
title:                "Travailler avec json"
html_title:           "PowerShell: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi? 

Le JSON (JavaScript Object Notation) est une façon super calme de stocker et partager des données. Les programmeurs adorent ça parce que c’est simple, léger, et facile à comprendre. 

## Comment faire: 

**Note: Assurez-vous que vous possédez la dernière version de PowerShell avant de commencer!**

Pour créer un fichier JSON dans PowerShell, utilisez la commande ```New-Item``` et spécifiez l'extension ```.json```. Vous pouvez ensuite ajouter des données à votre fichier en utilisant des cmdlets tels que ```Add-Member``` et ```Set-Content```. 

Exemple: 

```PowerShell
New-Item -Path C:\Users\Utilisateur\Documents\exemples -Name monfichier.json -ItemType File

$monObjet = [PSCustomObject]@{
    nom = "Jean"
    âge = 25
    ville = "Paris"
}
$monObjet | Add-Member -MemberType NoteProperty -Name "job" -Value "Développeur"
$monObjet | Set-Content C:\Users\Utilisateur\Documents\exemples\monfichier.json
```

Pour lire et manipuler des données JSON dans PowerShell, utilisez la commande ```ConvertFrom-Json``` et accédez aux propriétés en utilisant la notation avec un point. 

Exemple: 

```PowerShell
$monObjet = Get-Content C:\Users\Utilisateur\Documents\exemples\monfichier.json | ConvertFrom-Json

$monObjet.nom
$monObjet | Select-Object nom, ville
```

## Deep Dive: 

Bien que le JSON ait été initialement utilisé pour les applications en JavaScript, il est maintenant pris en charge par de nombreux langages de programmation, dont PowerShell. Cela en fait un format de données très pratique pour la communication entre différentes applications ou systèmes. 

Il existe d'autres formats de données tels que le XML qui peuvent également être utilisés pour le stockage et le partage de données, mais JSON est plus léger et facile à lire et à écrire pour les humains. 

En termes d'implémentation, PowerShell utilise le module ```ConvertTo-Json``` et ```ConvertFrom-Json``` pour transformer des objets en JSON et vice-versa. Il est également possible de manipuler des données JSON en utilisant la méthode ```.NET``` ```System.Text.Json```. 

## See Also: 

Consultez ces liens pour en savoir plus sur les travaux avec JSON en PowerShell: 

- Documentation officielle: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/convertto-json?view=powershell-7.1 
- Tutoriel en 5 minutes: https://adamtheautomator.com/working-with-json-powershell/ 
- Utilisation de la méthode ```.NET```: https://4sysops.com/archives/powershell-parsing-json-using-the-net-framework/