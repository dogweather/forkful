---
title:    "PHP: Obtenir la date actuelle"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi

La fonction permettant d'obtenir la date actuelle est probablement l'une des tâches les plus simples en programmation PHP, mais elle est également très utile. Que vous ayez besoin de vérifier la date pour un système de réservation ou pour tout autre type de caractéristique, il est important de comprendre comment obtenir la date actuelle dans votre code.

## Comment faire

Pour obtenir la date actuelle en PHP, vous pouvez utiliser la fonction prédéfinie "date()". Cette fonction prend deux paramètres, le premier étant le format de date souhaité et le second étant facultatif pour définir un timestamp spécifique.

Voici un exemple de code PHP pour obtenir la date actuelle au format jour/mois/année:

```
<?php
echo date("d/m/Y");
```

Le code ci-dessus affichera la date actuelle dans le format spécifié, par exemple "28/08/2021".

## Plongée en profondeur

La fonction "date()" utilise un timestamp interne pour obtenir la date actuelle, basé sur le fuseau horaire défini dans votre fichier de configuration PHP. Cela signifie que la date peut être différente selon l'endroit où votre serveur est situé. Vous pouvez utiliser la fonction "timezone_set()" pour définir un fuseau horaire spécifique si nécessaire.

La fonction "date()" peut également être utilisée pour afficher d'autres informations de date et d'heure, telles que l'heure, les minutes, les secondes, etc. Vous pouvez consulter la documentation PHP pour en savoir plus sur les différents formats disponibles.

## Voir aussi

- Documentation officielle PHP sur la fonction "date()": https://www.php.net/manual/fr/function.date.php
- Tutoriel sur les fonctions de date et d'heure en PHP: https://www.php.net/manual/fr/datetime.formats.php
- Article sur la gestion des fuseaux horaires en PHP: https://www.php.net/manual/fr/timezones.php