---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Obtenir la date courante en programmation PHP consiste à récupérer la date et l'heure système de l'ordinateur. Les programmeurs le font souvent pour suivre les événements, timestamp les actions utilisateurs ou programmer des tâches à réaliser à des moments précis.

## Comment faire:

Utiliser la fonction `date` pour récupérer la date courante. Voici un exemple illustrant comment le faire:

```php
<?php
    echo date("Y-m-d H:i:s");
 ?>
```

Ce code récupère et affiche la date sous le format année-mois-jour heures:minutes:secondes.

## Plongée au cœur:

1. **Contexte historique**: PHP offre plusieurs fonctions pour manipuler les dates et les heures depuis sa formation initiale. `date` est présent en PHP depuis sa première version.

2. **Alternatives**: Une autre façon d'obtenir la date courante est d'utiliser la classe `DateTime` introduite avec la version 5.2.0 de PHP:
   
```php
<?php 
    $date = new DateTime();
    echo $date->format("Y-m-d H:i:s"); 
?>
```

3. **Détails d'implémentation**: `date` ou `DateTime::format` récupère la date et l'heure en fonction du fuseau horaire défini dans le fichier `php.ini`, ou si aucun n'est défini, il utilisera le fuseau horaire du serveur.

## Voir aussi:

Pour plus d'informations, consultez les pages de documentation suivantes: 

1. PHP `date`: [Voici le lien](https://www.php.net/manual/fr/function.date.php)
2. PHP `DateTime`: [Voici le lien](https://www.php.net/manual/fr/class.datetime.php)
3. Définir le fuseau horaire avec PHP: [Voici le lien](https://www.php.net/manual/fr/datetime.settimezone.php) 

Et voilà, vous savez maintenant comment obtenir la date courante en PHP. Bonne programmation à vous!