---
date: 2024-01-26 00:55:14.503733-07:00
description: "La gestion des erreurs en PHP consiste \xE0 g\xE9rer et r\xE9pondre\
  \ \xE0 des conditions qui perturbent le d\xE9roulement normal d\u2019un programme,\
  \ telles que des fichiers\u2026"
lastmod: '2024-03-11T00:14:31.837088-06:00'
model: gpt-4-1106-preview
summary: "La gestion des erreurs en PHP consiste \xE0 g\xE9rer et r\xE9pondre \xE0\
  \ des conditions qui perturbent le d\xE9roulement normal d\u2019un programme, telles\
  \ que des fichiers\u2026"
title: Gestion des erreurs
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
La gestion des erreurs en PHP consiste à gérer et répondre à des conditions qui perturbent le déroulement normal d’un programme, telles que des fichiers manquants ou des entrées de données incorrectes. Les programmeurs traitent les erreurs pour éviter les plantages et offrir une expérience plus fluide aux utilisateurs.

## Comment faire :
En PHP, vous pouvez gérer les erreurs à l'aide de blocs `try-catch`, et vous pouvez personnaliser le processus avec des gestionnaires d'erreur personnalisés et des exceptions.

```php
// Exemple de base de try-catch
try {
  // Faire quelque chose de risqué
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // Gérer l'erreur
  echo "Erreur : " . $e->getMessage();
}

// Définition d'un gestionnaire d'erreurs personnalisé
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// Utilisation des exceptions
class MyException extends Exception {}

try {
  // Faire quelque chose et lancer une exception personnalisée
  throw new MyException("Erreur personnalisée !");
} catch (MyException $e) {
  // Gérer l'exception personnalisée
  echo $e->getMessage();
}

// Sortie d'exemple :
// Erreur : fopen(nonexistentfile.txt): échec de l'ouverture du flux : Aucun fichier ou dossier de ce type
// Erreur personnalisée !
```

## Plongée en profondeur
Autrefois, les erreurs PHP étaient plus des avertissements et des notices qui n'arrêtaient pas l'exécution du script. En mûrissant, le langage a adopté une gestion des erreurs orientée objet plus robuste via la classe Exception introduite dans PHP 5. Plus tard, PHP 7 est arrivé avec des classes Error qui ont enfin différencié les erreurs et les exceptions.

Avant les blocs `try-catch`, PHP utilisait `set_error_handler()` pour traiter les erreurs. `try-catch` est plus propre et plus moderne. Mais les gestionnaires d'erreurs personnalisés ont toujours leur place, en particulier pour le code existant ou lorsque vous avez besoin d'attraper ce qui serait normalement des erreurs non exceptionnelles.

L'interface `Throwable` dans PHP 7+ signifie que, qu'il s'agisse d'une Error ou d'une Exception, vous pouvez intercepter les deux. C'est pratique car vous ne manquez plus les erreurs d'exécution critiques, qui étaient plus difficiles à suivre auparavant.

Les alternatives en dehors des mécanismes intégrés de PHP incluent des bibliothèques et des frameworks qui viennent avec leurs propres systèmes de gestion des erreurs, offrant plus de fonctionnalités comme la journalisation des erreurs dans des fichiers ou l'affichage de pages d'erreur conviviales pour l'utilisateur.

## Voir aussi
- Documentation officielle PHP sur les Exceptions : https://www.php.net/manual/fr/language.exceptions.php
- PHP The Right Way sur le rapport d'erreurs : https://phptherightway.com/#error_reporting
- Manuel PHP sur la Gestion des Erreurs : https://www.php.net/manual/fr/book.errorfunc.php
