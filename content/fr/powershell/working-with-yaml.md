---
title:                "Travailler avec yaml"
html_title:           "PowerShell: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Travailler avec YAML, c'est un moyen de stocker et de transférer des données structurées de manière facilement lisible. En tant que programmeurs, nous utilisons YAML pour faciliter l'échange de données entre différentes applications et langages de programmation.

## Comment faire?

Les blocs de code PowerShell suivants illustrent comment travailler avec YAML :

- Convertir des données YAML en objets PowerShell :
```PowerShell
$data = @"
    animal: chat
    couleur: noir
"@
$objet = ConvertFrom-Yaml -InputObject $data
```
Output : 
```
animal  couleur
______  _______
Chat    Noir
```

- Convertir un objet PowerShell en données YAML :
```PowerShell
$objet = [PSCustomObject]@{
    fruit = "pomme"
    quantite = 4
}
$donnees = ConvertTo-Yaml -InputObject $objet
```
Output : 
```
fruit: pomme
quantite: 4
```

## Plongée en profondeur

### Contexte historique

YAML, qui signifie "YAML Ain't Markup Language", a été créé en 2001 par un groupe de programmeurs pour répondre aux limitations de formats de données tels que XML et JSON. Il est maintenant utilisé en tant que langage de configuration dans de nombreuses applications.

### Alternatives

D'autres formats de données populaires comme XML et JSON peuvent également être utilisés pour stocker des données structurées. Cependant, YAML a l'avantage d'être plus facile à lire et à écrire pour les humains.

### Détails d'implémentation

PowerShell utilise le module "PowerShellYaml" pour prendre en charge la conversion de données YAML en objets PowerShell et vice versa. Ce module peut être téléchargé à partir de la galerie PowerShell.

## Voir aussi

Pour en savoir plus sur l'utilisation de YAML avec PowerShell, consultez les liens suivants :

- Documentation officielle de PowerShellYaml : https://docs.microsoft.com/powershell/module/powershellyaml/
- Tutoriel vidéo sur l'utilisation de YAML avec PowerShell : https://www.youtube.com/watch?v=CE1VKLqUOAo 
- Exemples de scénarios d'utilisation de YAML avec PowerShell : https://devblogs.microsoft.com/powershell/working-with-yaml-in-powershell/