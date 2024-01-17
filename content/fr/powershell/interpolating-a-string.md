---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "PowerShell: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Interpoler une chaîne de caractères en PowerShell, c'est utiliser des variables ou des expressions pour remplacer dynamiquement des parties de la chaîne. Les programmeurs le font pour rendre leur code plus concis et plus flexible, en évitant de devoir répéter plusieurs fois la même chaîne.

## Comment faire:
Pour interpoler une chaîne en PowerShell, il suffit d'utiliser des guillemets doubles (" ") et de mettre les variables ou expressions entre $(). Par exemple, ```"Bonjour $($nom)"``` affichera la chaîne "Bonjour" suivie de la valeur contenue dans la variable $nom. Voici un exemple plus concret:

```PowerShell
$nom = "Emma"
"Pas facile d'être $($nom) quand on est interprété en PowerShell!"
```

Résultat: Pas facile d'être Emma quand on est interprété en PowerShell!

## Plongée en profondeur:
L'interpolation de chaînes existe depuis longtemps en informatique et a été populaireisée par le langage Perl. Avant PowerShell, le langage de script Batch permettait déjà de remplacer des variables dans les chaînes en utilisant le symbole %. D'autres alternatives à l'interpolation de chaînes sont l'utilisation de la concaténation de chaînes avec l'opérateur + ou l'utilisation de chaînes formatées avec la méthode .format(). 

## Voir aussi:
- [Documentation officielle de Microsoft sur l'interpolation de chaînes en PowerShell](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7.1)
- [Article de blog sur l'interpolation de chaînes en PowerShell](https://www.hybridcloudtips.com/powershell/powershell-string-interpolation/)
- [Vidéo explicative sur l'interpolation de chaînes en PowerShell](https://www.youtube.com/watch?v=Nqzd1ZE6Eoo)