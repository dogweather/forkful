---
title:                "Écrire sur l'erreur standard"
aliases:
- /fr/php/writing-to-standard-error/
date:                  2024-02-03T19:34:01.419082-07:00
model:                 gpt-4-0125-preview
simple_title:         "Écrire sur l'erreur standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire sur l'erreur standard (stderr) en PHP consiste à diriger les messages d'erreur ou les diagnostics séparément de la sortie standard (stdout), permettant aux développeurs de mieux gérer leurs flux de sortie pour le débogage et la journalisation. Les programmeurs utilisent cette technique pour s'assurer que les messages d'erreur n'interfèrent pas avec la sortie du programme, rendant ainsi plus facile la surveillance et le dépannage des applications.

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
