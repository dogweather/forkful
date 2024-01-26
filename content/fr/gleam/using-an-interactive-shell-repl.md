---
title:                "Utilisation d'une console interactive (REPL)"
date:                  2024-01-26T04:14:08.292338-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'une console interactive (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Un REPL, abréviation de Read-Eval-Print Loop (Boucle de Lecture-Évaluation-Affichage), est un outil de programmation permettant d'exécuter interactivement du code et de voir les résultats instantanément. Les programmeurs l'utilisent pour expérimenter, déboguer, ou apprendre un nouveau langage à la volée comme Gleam.

## Comment faire :

Gleam n'inclut actuellement pas de REPL dans sa distribution standard. Cependant, vous pouvez expérimenter avec du code Gleam en utilisant le shell Erlang existant car Gleam compile en bytecode Erlang. Voici comment :

1. Compilez votre code Gleam en Erlang.
```plaintext
gleam build
```

2. Démarrez le shell Erlang.
```plaintext
erl -pa ebin
```

3. Appelez vos fonctions Gleam (en supposant que vous avez un module nommé `my_mod` et une fonction `my_fun`).
```erlang
my_mod:my_fun().
```

Vous devriez voir la sortie de votre fonction affichée dans le shell.

## Plongée profonde

REPL incarne l'esprit dynamique et exploratoire de nombreuses langues de programmation fonctionnelles, remontant au REPL de LISP dans les années 1960. Comparativement, d'autres systèmes comme `ipython` de Python ou `irb` de Ruby offrent des expériences similaires pour leurs communautés.

Bien que Gleam n'ait pas encore de REPL natif, l'utilisation du shell Erlang reste une solution astucieuse. Les capacités du shell Erlang viennent de la VM BEAM, la machine virtuelle alimentant l'écosystème Erlang, qui inclut Elixir, LFE et Gleam.

Les alternatives aux REPLs dans l'écosystème Gleam pourraient inclure l'écriture de cas de test ou l'utilisation de compilateurs en ligne et de terrains de jeu de code qui supportent Gleam, pour tester des bouts de code en dehors d'une configuration de projet complète.

La mise en œuvre d'un REPL Gleam dédié se heurte principalement à la nature compilée de Gleam et à l'exécution d'Erlang, où le remplacement à chaud du code est la norme. Tout REPL Gleam futur devrait réconcilier la typage statique du langage avec l'environnement d'exécution dynamique qu'un REPL attend.

## Voir également

- Documentation officielle de Gleam : https://gleam.run/book/
- Documentation du shell Erlang : http://erlang.org/doc/man/erl.html
- Un terrain de jeu de compilation en ligne pour Gleam : https://gleam.run/compiler/