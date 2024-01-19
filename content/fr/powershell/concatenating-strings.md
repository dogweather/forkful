---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concaténation de chaînes de caractères en PowerShell #

## Quoi et Pourquoi ? ##

La concaténation de chaînes est l'action de relier deux chaînes de caractères ou plus en une seule. Les programmeurs le font pour manipuler et présenter des données de manière plus efficace et affinée.

## Comment faire : ##

Concaténer des chaînes en PowerShell est assez simple. Jetons un coup d'oeil à quelques exemples.

```PowerShell
$premiereChaine = "calmement,"
$deuxiemeChaine = " coder."
$chaineFinale = $premiereChaine + $deuxiemeChaine
Write-Output $chaineFinale
```
**Sortie :**
```
calmement, coder.
```

Utilisez également la méthode Format. Regardons un autre exemple :

```PowerShell
$nom = "Alex"
$salutation = "Salut, {0}!"
$salutationFormatee = [String]::Format($salutation, $nom)
Write-Output $salutationFormatee
```

**Sortie :**
```
Salut, Alex!
```

## Immersion profonde ##

Historiquement, la concaténation de chaînes a toujours été un élément fondamental de la programmation. En revanche, il est important de noter que la concaténation peut être coûteuse en termes de performance, en particulier avec de grandes chaînes ou de nombreuses opérations.

Des alternatives existent, comme les littéraux de modèle (aussi connus comme chaînes interpolées) qui simplifient la syntaxe:

```PowerShell
$nom = "Alex"
$salutation = "Salut, $nom!"
Write-Output $salutation
```

**Sortie :**
```
Salut, Alex!
```

Aussi, implémentation de la concaténation en PowerShell accorde indirectement de la flexibilité, permettant différentes approches pour atteindre le même objectif.

## Voir Also ##

Posez un coup d'oeil à ces ressources pour plus d'informations :

1. Documentation Microsoft sur les opérations de chaîne de caractères : [Lien](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/strings/)
2. Article sur la manipulation de chaînes en PowerShell : [Lien](https://devblogs.microsoft.com/scripting/manipulating-strings-with-powershell/)
3. Documentation Microsoft sur la méthode Format : [Lien](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.format?view=net-5.0)