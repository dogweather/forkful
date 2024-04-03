---
date: 2024-01-26 04:17:52.108983-07:00
description: "Comment faire : \xC0 l'heure actuelle, Rust n'a pas de REPL officiel\
  \ livr\xE9 avec. Vous pouvez utiliser des outils tiers comme `evcxr_repl`. Installez-le\
  \ avec\u2026"
lastmod: '2024-03-13T22:44:57.483578-06:00'
model: gpt-4-0125-preview
summary: "\xC0 l'heure actuelle, Rust n'a pas de REPL officiel livr\xE9 avec."
title: Utilisation d'une console interactive (REPL)
weight: 34
---

## Comment faire :
À l'heure actuelle, Rust n'a pas de REPL officiel livré avec. Vous pouvez utiliser des outils tiers comme `evcxr_repl`. Installez-le avec Cargo :

```sh
cargo install evcxr_repl
```

Ensuite, exécutez le REPL :

```sh
evcxr
```

À l'intérieur, testez du code Rust :

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

La sortie devrait être :

```
5 + 3 = 8
```

## Plongée Profonde
L'éthique de Rust est centrée autour de la sécurité et de la performance, qui sont généralement associées aux langues compilées à l'avance, et moins aux langues interprétées, conviviales pour un REPL. Historiquement, des langues comme Python ou Ruby ont donné la priorité à la présence d'un REPL pour un retour immédiat, mais n'ont pas été conçues avec les tâches de niveau système à l'esprit.

Malgré l'absence d'un REPL officiel dans Rust, quelques alternatives comme `evcxr_repl` ont émergé. Ces projets ne se contentent pas de forcer Rust dans un REPL ; ils tissent intelligemment ensemble le cycle de compilation et d'exécution du langage dans une session interactive. Le REPL compile le code en arrière-plan et exécute le binaire, capturant la sortie. De cette façon, il préserve les avantages de performance de Rust tout en offrant cette expérience interactive.

Il y a une discussion en cours dans la communauté Rust au sujet du support officiel d'un REPL, et avec chaque itération du langage, nous voyons plus de sophistication dans les outils qui pourraient éventuellement conduire à une solution native.

## Voir Aussi
Pour plus d'infos et d'autres outils :
- Dépôt GitHub d'Evcxr REPL : [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, une façon en ligne de tester le code Rust : [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Discussion sur la fonctionnalité REPL dans le langage Rust : [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
