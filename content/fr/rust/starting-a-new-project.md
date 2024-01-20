---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Commencer un nouveau projet, c'est créer une toute nouvelle application à partir de zéro. C'est une façon pour les programmeurs d'explorer des idées nouvelles, de résoudre des problèmes uniques et de dynamiser leur créativité.

## Comment faire:

Nous utiliserons l'outil Cargo, intégré dans Rust, pour créer notre nouveau projet. 

```Rust
// Pour lancer un nouveau projet, utilisez la commande suivante dans votre terminal.
cargo new mon_projet
```

Cette commande crée une nouvelle application Rust nommée `mon_projet` dans un sous-répertoire de votre emplacement actuel.

## Plongée profonde

**Contexte historique:** Rust est lancé en 2010 par Graydon Hoare, avec l'aide de Mozilla. L'objectif était de fournir un langage de programmation orienté système, sûr en termes de mémoire, mais sans compromettre la performance.

**Alternatives:** D'autres outils de structure de projet existent comme Kickstart qui permet de créer rapidement des projets Rust simples. Mais pour les débutants, utiliser Cargo est plus judicieux car il est intégré dans l'écosystème Rust.

**Détails de l'implémentation:** L'utilisation de `cargo new` crée un dossier contenant notamment un fichier `Cargo.toml`, c'est le manifeste du package. Il contient les métadonnées telles que le nom, la version, les auteurs du package et les dépendances.

## Voir aussi

- La documentation officielle de Rust : https://doc.rust-lang.org/book/ch01-03-hello-cargo.html
- Un tutoriel en ligne sur comment commencer à programmer en Rust : https://www.rust-lang.org/learn/get-started
- Un forum dédié à la programmation en Rust : https://users.rust-lang.org/