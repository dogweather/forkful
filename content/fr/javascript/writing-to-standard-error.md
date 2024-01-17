---
title:                "Écrire sur la sortie d'erreur standard"
html_title:           "Javascript: Écrire sur la sortie d'erreur standard"
simple_title:         "Écrire sur la sortie d'erreur standard"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?

Écrire dans la sortie d'erreur standard, communément appelée stderr, est une action couramment réalisée par les programmeurs en JavaScript. Cela permet d'afficher des messages d'erreur sur la console plutôt que sur la sortie standard classique (stdout). Les programmeurs font cela pour déboguer leur code et s'assurer que leur programme fonctionne correctement.

# Comment faire: 

Voici un exemple simple de code en JavaScript pour écrire dans la sortie d'erreur standard (stderr) :

```Javascript
console.error("Une erreur s'est produite !");
```

Cela produira le résultat suivant dans la console :

```
Une erreur s'est produite !
```

Vous pouvez également utiliser la méthode `console.warn()` pour écrire des avertissements dans la sortie d'erreur standard :

```Javascript
console.warn("Attention : une erreur pourrait se produire !");
```

Ce qui affichera le résultat suivant dans la console :

```
Attention : une erreur pourrait se produire !
```

# Plongée en profondeur: 

L'écriture dans la sortie d'erreur standard a été introduite pour la première fois dans le langage de programmation C dans les années 1970. Depuis lors, cette méthode est devenue une pratique courante pour les programmeurs dans différents langages de programmation.

Il existe également d'autres alternatives pour afficher les messages d'erreur, comme l'utilisation de console.log() ou la création de fichiers de journalisation. Cependant, écrire dans la sortie d'erreur standard est souvent considéré comme une méthode plus efficace pour le débogage et la gestion des erreurs.

Pour implémenter l'écriture dans la sortie d'erreur standard en JavaScript, vous pouvez utiliser la méthode `console.error()` ou `console.warn()` comme indiqué précédemment. Il est également possible d'utiliser `process.stderr.write()` pour écrire directement dans le flux de sortie d'erreur standard.

# Voir aussi:

- [Documentation de console.error() sur MDN](https://developer.mozilla.org/fr/docs/Web/API/Console/error)
- [Documentation de process.stderr sur Node.js](https://nodejs.org/api/process.html#process_process_stderr)
- [Article sur la différence entre console.log() et console.error()](https://wdevotional.com/console-log-vs-console-error-which-one-to-use-when-adbed975727f)