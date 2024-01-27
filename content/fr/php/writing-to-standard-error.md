---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire sur l'erreur standard (stderr) permet de séparer les messages d'erreur du flux de sortie principal. Les programmeurs le font pour faciliter le débogage et la gestion des logs.

## Comment faire :
```PHP
<?php
// Écrire un message simple sur stderr
file_put_contents('php://stderr', "Il y a eu une erreur!\n");

// Utiliser les flux pour un message plus complexe
$stderr = fopen('php://stderr', 'w');
fwrite($stderr, "Attention : erreur critique!\n");
fclose($stderr);
?>
```
Sortie à attendre dans la console/sur stderr :
```
Il y a eu une erreur!
Attention : erreur critique!
```

## Plongée profonde
Historiquement, la séparation entre sortie standard et erreur standard vient de Unix. En PHP, les alternatives incluent les fonctions comme `error_log()` pour envoyer des erreurs aux différents gestionnaires de journaux, ou des extensions comme Monolog pour une gestion d'erreur plus avancée. Pour écriture sur stderr, PHP utilise des flux qui sont implémentés à un niveau plus bas dans le langage.

## Voir Aussi
- Documentation PHP sur les flux: https://www.php.net/manual/fr/book.stream.php
- Comment utiliser Monolog pour la gestion des logs: https://github.com/Seldaek/monolog
- Documentation PHP sur la fonction error_log: https://www.php.net/manual/fr/function.error-log.php
