---
title:                "Lancement d'un nouveau projet"
date:                  2024-01-20T18:04:14.131954-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lancement d'un nouveau projet"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Démarrer un nouveau projet, c'est comme ouvrir un livre vierge : vous avez une toile inédite pour écrire votre code. Les programmeurs lancent de nouveaux projets pour transformer des idées en réalité, pour résoudre des problèmes ou juste pour s'amuser.

## Comment faire :

Pour commencer, on utilise Cargo, l'outil de gestion de paquets et de système de build de Rust. Voici comment initier un projet :

```Rust
// Ouvrez un terminal et tapez :
cargo new mon_projet

// Entrons dans le répertoire du projet :
cd mon_projet

// Exécutons le projet :
cargo run

// Vous devriez voir quelque chose comme :
   Compiling mon_projet v0.1.0 (/chemin/vers/mon_projet)
    Finished dev [unoptimized + debuginfo] target(s) in 0.5 secs
     Running `target/debug/mon_projet`
Hello, world!
```

## Deep Dive

Cargo n'est pas le premier outil de sa catégorie, mais il embrasse Rust comme aucun autre avec sa simplicité et son efficacité. Avant Cargo, les programmeurs Rust utilisaient make et d'autres outils de build qui nécessitaient plus de configuration. Avec Cargo, la gestion des dépendances et la compilation sont simplifiées. Vous pouvez même publier votre bibliothèque sur crates.io avec une simple commande (`cargo publish`), faisant de la réutilisation des paquets une partie intégrante de l'écosystème Rust.

Cargo permet aussi d'organiser le projet avec des conventions, par exemple en mettant le code source dans `src` et les fichiers de configuration dans le répertoire racine, ce qui encourage la cohérence entre les projets Rust. Alternativement, les développeurs habitués à d'autres langages et systèmes de build pourraient trouver cela contraignant, mais la plupart s'adaptent vite aux avantages que cela apporte.

## Voir aussi

Pour approfondir vos connaissances sur Cargo et la création de projets Rust :

- [La page officielle de Cargo](https://doc.rust-lang.org/cargo/)
- [Le livre "The Rust Programming Language"](https://doc.rust-lang.org/book/ch01-03-hello-cargo.html) - contient un chapitre sur Cargo.
- [Crates.io](https://crates.io/) - pour explorer les bibliothèques publiées par la communauté Rust.
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/) - offre des exemples interactifs pour apprendre Rust en pratiquant.
