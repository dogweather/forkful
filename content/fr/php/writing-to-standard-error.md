---
title:                "Écrire vers l'erreur standard"
html_title:           "PHP: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'écriture sur la sortie d'erreur standard est une méthode utilisée par les programmeurs pour afficher des messages d'erreur ou de débogage lors de l'exécution de leur code. Elle est principalement utilisée pour diagnostiquer et résoudre les erreurs dans le code, afin de faciliter le processus de développement.

Les programmeurs utilisent l'écriture sur la sortie d'erreur standard pour être en mesure d'identifier rapidement les erreurs et les bugs dans leur code sans avoir à passer par des processus de débogage plus complexes. Cela leur permet de gagner du temps et d'améliorer l'efficacité de leur travail.

## Comment:

Voici un exemple simple de code PHP utilisant l'écriture sur la sortie d'erreur standard:

```PHP
<?php
// code qui génère une erreur
echo "Bonjour";
// écriture sur la sortie d'erreur standard
stderr("Une erreur s'est produite");
?>
```

Voici le résultat de l'exécution de ce code:

```
Bonjour
Une erreur s'est produite
```

## Plongée en profondeur:

L'écriture sur la sortie d'erreur standard a été initialement développée pour les systèmes UNIX, où les messages d'erreur étaient envoyés à stderr au lieu de stdout (la sortie standard). Cela permettait d'identifier plus facilement les erreurs dans les processus en cours d'exécution.

Il existe d'autres méthodes pour afficher des messages d'erreur, telles que la journalisation ou l'utilisation de la fonction PHP `error_log()`. Cependant, l'écriture sur la sortie d'erreur standard reste une méthode populaire pour les programmeurs en raison de sa simplicité et de sa rapidité.

Pour implémenter l'écriture sur la sortie d'erreur standard en PHP, il suffit d'utiliser la fonction `stderr()` et de lui passer le message d'erreur en tant que paramètre.

## Voir aussi:

- [Article sur l'écriture sur la sortie d'erreur standard en PHP](https://www.php.net/manual/fr/wrappers.php)
- [Documentation officielle sur la fonction `error_log()` en PHP](https://www.php.net/manual/fr/function.error-log.php)
- [Article sur le débogage en PHP](https://www.phptherightway.com/#debugging)