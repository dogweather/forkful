---
title:                "Écrire sur la sortie standard"
html_title:           "Rust: Écrire sur la sortie standard"
simple_title:         "Écrire sur la sortie standard"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Rust, comme de nombreux autres langages de programmation, permet d'écrire sur la sortie standard d'un programme, également appelée standard output (stdout). Cependant, parfois un programmeur peut avoir besoin d'écrire sur une autre sortie, appelée standard error (stderr). Cette sortie est utilisée pour les messages d'erreur ou les avertissements, afin qu'ils puissent être distingués des résultats de sortie normaux. Cela peut aider les développeurs à déboguer et à corriger les erreurs plus facilement.

## Comment faire:

Pour écrire sur la sortie standard d'erreur en Rust, il suffit d'utiliser la fonction `eprintln!()` et de lui passer la chaîne de caractères que vous souhaitez afficher. Par exemple:
```Rust
fn main() {
    eprintln!("Erreur: variable non définie);
}
```
Cela affichera "Erreur: variable non définie" sur la sortie standard d'erreur.

Un autre moyen de récupérer les erreurs et les avertissements est d'utiliser la macro `std::io::stderr`. Cette macro renvoie un objet pouvant être utilisé pour écrire sur la sortie standard d'erreur. Voici un exemple:
```Rust
use std::io::Write;
fn main() {
    let mut stderr = std::io::stderr();
    stderr.write(b"Avertissement: fichier manquant").unwrap();
}
```
Cela affichera "Avertissement: fichier manquant" sur la sortie standard d'erreur.

## Plongée en profondeur:

L'écriture sur la sortie standard d'erreur existe depuis le début de l'informatique. Il a été introduit pour aider les développeurs à déboguer des programmes et à signaler des erreurs. Dans certains langages, comme C, la fonction `fprintf()` est spécialement conçue pour écrire sur la sortie standard d'erreur. Cependant, dans Rust, la fonction `eprintln!()` fournit une syntaxe plus simple et plus directe pour écrire sur la sortie d'erreur.

Il existe également des alternatives à l'utilisation de la sortie standard d'erreur. Par exemple, un programmeur peut choisir d'utiliser des bibliothèques de journalisation pour gérer les messages d'erreur et les avertissements. Cela peut être utile pour les programmes plus volumineux, où la gestion des messages de sortie devient plus complexe.

Enfin, il est intéressant de noter que l'implémentation de l'écriture sur la sortie standard d'erreur peut varier en fonction du système d'exploitation utilisé. Par exemple, sur Windows, la sortie standard d'erreur est généralement redirigée vers la fenêtre de la console, tandis que sur Linux, elle est généralement redirigée vers le fichier `stderr`.

## Voir aussi:

- [Documentation de Rust sur les fonctions d'affichage](https://doc.rust-lang.org/std/macro.eprint.html)
- [Une alternative populaire à la sortie standard d'erreur: la bibliothèque de journalisation log4rs](https://github.com/estk/log4rs)
- [Explication détaillée de la différence entre la sortie standard et la sortie standard d'erreur](https://stackoverflow.com/questions/2342826/what-is-the-difference-between-stdout-and-stderr-in-c)