---
title:                "Journalisation"
aliases:
- /fr/php/logging.md
date:                  2024-01-26T01:07:09.884379-07:00
model:                 gpt-4-1106-preview
simple_title:         "Journalisation"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/logging.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La journalisation est en gros l'équivalent de tenir un journal pour votre code ; c'est l'action d'enregistrer des événements, des erreurs et d'autres points de données significatifs qui se produisent lorsque votre application fonctionne. Les programmeurs le font pour suivre ce qui se passe sous le capot, pour déboguer des problèmes et pour maintenir une piste d'audit pour des analyses ultérieures ou pour des raisons de conformité.

## Comment faire :

PHP est livré avec une fonction intégrée de journalisation d'erreurs qui est facile à utiliser. Il suffit d'insérer `error_log()` dans votre code pour envoyer un message aux journaux de votre serveur. Vous pouvez également la personnaliser pour écrire dans un fichier spécifique.

```php
<?php
// Journaliser un simple message d'information
error_log("C'est une entrée de journal d'info.");

// Journaliser un message d'erreur
error_log("C'est une entrée de journal d'erreur.", 0);

// Journaliser dans un fichier spécifié
file_put_contents('/chemin/vers/votre/custom.log', "Une entrée de journal personnalisée.\n", FILE_APPEND);

// Utiliser Monolog pour une journalisation structurée
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Créer le journaliseur
$logger = new Logger('nom');
// Maintenant, ajoutez quelques gestionnaires
$logger->pushHandler(new StreamHandler('/chemin/vers/votre/monolog.log', Logger::WARNING));

// Vous pouvez maintenant utiliser votre journaliseur
$logger->warning('Ceci est un log d'avertissement!');
$logger->error('Ceci est un log d'erreur!');
?>
```

Cela sortira vos journaux vers le journal du serveur ou votre fichier spécifié en format texte brut.

## Plongée approfondie :

Historiquement, les développeurs PHP se reposaient sur la fonction `error_log()` ou sur les journaux d'Apache/Nginx pour repérer les problèmes, mais cela peut être chaotique avec la nécessité de parser des fichiers texte brut et sans moyen facile de les filtrer ou de les trier. Entrez dans les bibliothèques de journalisation telles que Monolog, qui ont inauguré l'ère de la journalisation structurée en PHP. Ces solutions vous donnent un meilleur contrôle en offrant plusieurs canaux de journalisation, des niveaux de gravité et une sortie formatée (comme le JSON, qui est un rêve pour le parsing programmatique).

Les alternatives à Monolog incluent Log4php, KLogger et Log4php d'Apache. En termes d'implémentation, une journalisation robuste nécessite non seulement de déverser des données n'importe où, mais aussi de prendre en compte des éléments tels que la rotation des journaux, les stratégies d'archivage et l'intégration avec des outils de surveillance pour être vraiment utiles.

Vous devriez garder à l'esprit l'[Interface de Journalisation PSR-3](https://www.php-fig.org/psr/psr-3/), qui décrit une interface commune pour les bibliothèques de journalisation, assurant l'interopérabilité et une manière cohérente d'accéder aux mécanismes de journalisation.

## Voir également :

- [Dépôt GitHub de Monolog](https://github.com/Seldaek/monolog)
- [Spécification de l'Interface de Journalisation PSR-3](https://www.php-fig.org/psr/psr-3/)
- [Documentation sur la Journalisation d'Erreurs PHP](https://www.php.net/manual/fr/function.error-log.php)
- [KLogger : Une Classe de Journalisation Simple Pour PHP](https://github.com/katzgrau/KLogger)
- [Log4php : Un cadre de journalisation polyvalent pour PHP](https://logging.apache.org/log4php/)

Mouillez-vous les pieds avec les fonctions intégrées, mais pour une approche plus maintenable et évolutif, envisagez de prendre le temps de vous familiariser avec une bibliothèque comme Monolog. Bonne journalisation !
