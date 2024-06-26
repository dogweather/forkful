---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:01.419082-07:00
description: "Comment faire : En PHP, \xE9crire sur stderr peut \xEAtre r\xE9alis\xE9\
  \ en utilisant la fonction `fwrite()` en conjonction avec la constante pr\xE9d\xE9\
  finie `STDERR`, qui\u2026"
lastmod: '2024-03-13T22:44:57.896495-06:00'
model: gpt-4-0125-preview
summary: "En PHP, \xE9crire sur stderr peut \xEAtre r\xE9alis\xE9 en utilisant la\
  \ fonction `fwrite()` en conjonction avec la constante pr\xE9d\xE9finie `STDERR`,\
  \ qui repr\xE9sente le flux de sortie d'erreur."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Comment faire :
En PHP, écrire sur stderr peut être réalisé en utilisant la fonction `fwrite()` en conjonction avec la constante prédéfinie `STDERR`, qui représente le flux de sortie d'erreur.

```php
<?php
// Écrire un message simple sur stderr.
fwrite(STDERR, "Ceci est un message d'erreur.\n");
```

Sortie d'exemple lorsque le script est exécuté depuis la ligne de commande:
```
Ceci est un message d'erreur.
```

Pour démontrer une utilisation plus pratique, considérez un scénario où vous analysez l'entrée d'un utilisateur et rencontrez des données inattendues :
```php
<?php
$input = 'données inattendues';

// Simuler une erreur dans le traitement de l'entrée utilisateur.
if ($input === 'données inattendues') {
    fwrite(STDERR, "Erreur : Entrée inattendue reçue.\n");
    exit(1); // Sortir avec une valeur non nulle pour indiquer une erreur.
}
```

Bien que les capacités intégrées de PHP pour gérer stderr soient généralement suffisantes, lorsque l'on traite avec des applications plus complexes ou que l'on souhaite intégrer la journalisation stderr avec des systèmes externes, des bibliothèques tierces comme Monolog peuvent être un allié puissant. Monolog est une bibliothèque de journalisation qui peut gérer stderr parmi de nombreuses autres cibles (fichiers, sockets, etc.).

Utilisation de Monolog pour écrire sur stderr :

D'abord, assurez-vous d'avoir installé Monolog via Composer :
```
composer require monolog/monolog
```

Ensuite, vous pouvez configurer Monolog pour utiliser le `StreamHandler` ciblant `php://stderr` :

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Créer un canal de log
$log = new Logger('nom');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// Ajouter un message de log sur stderr
$log->warning('Ceci est un message d'avertissement.');
```

Le code ci-dessus utilise Monolog pour envoyer un message d'avertissement sur stderr, ce qui est particulièrement utile pour les applications nécessitant des configurations de journalisation détaillées ou une surveillance de log externe.
