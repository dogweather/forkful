---
date: 2024-01-26 04:16:27.182841-07:00
description: "Un shell interactif, ou REPL (Read-Eval-Print Loop, Boucle de Lire-\xC9\
  valuer-Imprimer), vous permet d'\xE9crire et d'ex\xE9cuter du code PHP \xE0 la vol\xE9\
  e. C'est\u2026"
lastmod: '2024-03-13T22:44:57.880348-06:00'
model: gpt-4-0125-preview
summary: "Un shell interactif, ou REPL (Read-Eval-Print Loop, Boucle de Lire-\xC9\
  valuer-Imprimer), vous permet d'\xE9crire et d'ex\xE9cuter du code PHP \xE0 la vol\xE9\
  e."
title: Utilisation d'une console interactive (REPL)
weight: 34
---

## Comment faire :
Lancez le REPL PHP en exécutant `php -a` dans votre terminal. Voici un aperçu de son fonctionnement :

```php
php > echo "Bonjour, Monde !";
Bonjour, Monde !
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Vous pouvez aussi définir des fonctions :

```php
php > function somme($a, $b) { return $a + $b; }
php > echo somme(5, 10);
15
```

## Plongée Profonde
Les REPL existent sous une forme ou une autre depuis les premiers jours de LISP dans les années 1960. Le shell interactif de PHP est moins avancé par rapport à ceux de langages comme Python ou JavaScript. Il ne conserve pas l'état entre les sessions et manque de fonctionnalités telles que l'auto-complétion. Pour un REPL PHP plus riche en fonctionnalités, envisagez des alternatives comme `psysh` ou `boris`. Ces shells tiers offrent de meilleurs outils d'introspection, la complétion par tabulation, et même un débogueur.

Sous le capot, le REPL de PHP fonctionne en compilant et exécutant chaque ligne de code au fur et à mesure de son entrée. Les limitations de cette approche deviennent évidentes avec des choses comme la redéclaration de classes, ce qui n'est pas possible dans la même session. C'est excellent pour des tests simples, mais cela peut devenir encombrant pour des tâches complexes.

## Voir Aussi
- [Manuel PHP - Shell interactif](https://www.php.net/manual/fr/features.commandline.interactive.php)
- [PsySH : Une console de développeur en temps réel, débogueur interactif et REPL pour PHP](https://psysh.org/)
- [Boris : Un petit REPL pour PHP](https://github.com/borisrepl/boris)
