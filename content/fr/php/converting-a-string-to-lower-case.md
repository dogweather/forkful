---
title:                "Convertir une chaîne en minuscules"
html_title:           "PHP: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi? 
Convertir une chaîne en minuscules signifie transformer toutes les lettres majuscules de cette chaîne en lettres minuscules. Cela peut être pratique pour normaliser les données d'entrée et faciliter leur comparaison.

## Comment faire:
Voici comment vous pouvez abaisser une chaîne en PHP avec la fonction `strtolower()`:

```PHP
<?php
$chaine = "BONJOUR TOUT LE MONDE!";
$chaineEnMinuscule = strtolower($chaine);

echo $chaineEnMinuscule;
?>
```

Résultat de ce code sera:

```PHP
bonjour tout le monde!
```

## Plongée en Profondeur
Historiquement, PHP a fourni `strtolower()` depuis la toute première version, il est donc bien établi et largement utilisé. Cependant, il est important de noter qu'il ne fonctionne pas bien avec les caractères non ASCII. Pour une internationalisation correcte, utilisez `mb_strtolower()` qui respecte l'encodage.

```PHP
<?php
$chaine = "BONJOUR ÉTÉ!";
$chaineEnMinuscule = mb_strtolower($chaine, 'UTF-8');

echo $chaineEnMinuscule;
?>
```

Résultat de ce code sera:

```PHP
bonjour été!
```

## Voir Aussi:
Pour plus d'informations sur les fonctions de chaînes en PHP, jetez un coup d'oeil aux ressources suivantes:

- Documentation PHP pour `strtolower()`: www.php.net/manual/fr/function.strtolower.php
- Documentation PHP pour `mb_strtolower()`: www.php.net/manual/fr/function.mb-strtolower.php
- Guide pour la gestion des chaînes en PHP : www.php.net/manual/fr/book.strings.php