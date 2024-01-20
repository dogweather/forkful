---
title:                "Convertir une chaîne en minuscules"
html_title:           "PHP: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Transformer une chaîne en minuscules avec PowerShell: un guide rapide

## Quoi & Pourquoi?
Transformer une chaîne en minuscules, aussi connu sous le nom de "minusculation", signifie convertir toutes les lettres majuscules d'une chaîne en lettres minuscules. Les programmeurs font cela pour uniformiser les données, ce qui est particulièrement utile pour les comparaisons de chaînes.

## Comment faire:
Voici comment vous pouvez convertir une chaîne en minuscules en utilisant PowerShell:

```PowerShell
# Déclarez la chaîne
$maChaine = "Bonjour, JE suis UN Programmeur"
# Convertir en minuscules
$maChaine_en_minuscules = $maChaine.ToLower()
```

Quand vous imprimez `$maChaine_en_minuscules`, vous obtiendrez:

```PowerShell
"bonjour, je suis un programmeur"
```

## Plongée Profonde:
La fonction `ToLower()` en PowerShell a été introduite dans la version 1.0, et reste la méthode la plus populaire pour la minusculation des chaînes.

Alternativement, si vous cherchez à appliquer cela sur un tableau de chaînes, vous pouvez utiliser la méthode `ForEach-Object`:

```PowerShell
$mesChaines = "CHAINE1", "CHAINE2", "CHAINE3"
$mesChaines_en_minuscules = $mesChaines | ForEach-Object { $_.ToLower() }
```

Derrière le rideau, quand `ToLower()` est appelé, PowerShell utilise le `TextInfo` courant de la culture système actuelle pour produire une version en minuscules de la chaîne, ce qui le rend respectueux de la culture.

## Voir Aussi:
Pour davantage de lectures sur ce sujet, voici quelques liens utiles en anglais:

- Documentation officielle Microsoft sur ToLower: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/tolower
- Respect de la culture avec ToLower et ToUpper: https://docs.microsoft.com/dotnet/standard/base-types/best-practices-strings