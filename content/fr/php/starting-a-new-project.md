---
title:                "Lancement d'un nouveau projet"
date:                  2024-01-20T18:04:07.151129-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lancement d'un nouveau projet"
programming_language: "PHP"
category:             "PHP"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Commencer un nouveau projet PHP, c'est partir sur une feuille blanche, coder quelque chose d'unique. Les programmeurs le font pour résoudre des problèmes, explorer des idées, ou construire des produits innovants.

## How to: (Comment faire :) 
Pour démarrer un projet PHP, vous devez installer PHP et un serveur web comme Apache ou Nginx. Utilisez Composer pour gérer les dépendances. Voilà un exemple de structure basique :

```php
<?php
// index.php - Point d'entrée du projet
require 'vendor/autoload.php';

echo "Bonjour, monde !";
?>
```

```
$ php -S localhost:8000
```

Visitez `http://localhost:8000` dans votre navigateur. Vous devriez voir le message : `Bonjour, monde !`

## Deep Dive (Plongée Profonde)
PHP, né en 1995, était un simple scripting language pour des tâches web. Aujourd'hui, PHP est beaucoup plus. Avec PHP 8, vous avez des fonctionnalités comme les arguments nommés et les types mix. Composer, introduit en 2012, est le gestionnaire de dépendances de facto. Pour des projets plus robustes, on utilise des frameworks comme Laravel, Symfony ou Laminas. Ces outils facilitent la structuration du code, l'accès aux bases de données, et le traitement des requêtes.

## See Also (Voir Aussi)
- La documentation officielle PHP: [php.net](https://www.php.net/)
- Composer, le gestionnaire de dépendances pour PHP: [getcomposer.org](https://getcomposer.org/)
- Laravel, un framework PHP populaire: [laravel.com](https://laravel.com/)
- Symfony, un ensemble de composants PHP et un framework: [symfony.com](https://symfony.com/)
- Laminas Project (anciennement Zend Framework): [getlaminas.org](https://getlaminas.org/)
