---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Écrire dans un fichier texte, c'est sauvegarder des données lisibles par l'homme. Les développeurs le font pour conserver la configuration, les logs ou tout autre information qui doit être récupérée ou analysée plus tard.

## Comment faire :

```PowerShell
# Écrire du texte dans un fichier nouveau ou existant
"Bonjour, monde!" | Out-File -FilePath .\exemple.txt

# Ajouter du texte à la fin d'un fichier existant
"Ajout à mon fichier texte" | Add-Content -Path .\exemple.txt

# Vérifier le contenu du fichier
Get-Content -Path .\exemple.txt
```

Sortie échantillon :
```
Bonjour, monde!
Ajout à mon fichier texte
```

## Plongée profonde :

À l'origine, les fichiers textes servaient à stocker des données de façon simple, sans nécessiter de structures de données complexes. PowerShell offre plusieurs cmdlets comme `Out-File`, `Add-Content` et `Set-Content` pour écrire dans des fichiers textes, chacun ayant des cas d'usages spécifiques. La distinction principale entre eux est la gestion du contenu existant et la performance en cas de gros volumes de données.

## Voir aussi :

- [Out-File documentation on Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file)
- [Get-Content documentation on Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
