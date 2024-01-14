---
title:                "Rust: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en herbe ou expérimenté à la recherche d'un langage de programmation moderne, performant et sûr, alors Rust est fait pour vous. L'une des fonctionnalités les plus intéressantes de Rust est sa capacité à écrire directement vers l'erreur standard, ce qui peut être très utile lors du débogage de votre code.

## Comment faire

Écrire vers l'erreur standard en Rust est très simple. Tout d'abord, vous devez importer la bibliothèque standard de Rust en utilisant l'instruction suivante : ```use std::io::Write;```. Cette instruction vous permet d'utiliser la fonction ```write!``` pour écrire vers l'erreur standard. Ensuite, vous pouvez utiliser la fonction ```write!``` en spécifiant l'erreur standard comme paramètre, suivi du message que vous souhaitez afficher.

Par exemple, si vous souhaitez afficher le message ```"Erreur : la variable x n'a pas été initialisée"```, vous pouvez utiliser la ligne de code suivante : ```write!(std::io::stderr(), "Erreur : la variable x n'a pas été initialisée");```. Cela écrira le message directement vers l'erreur standard.

Il est également possible d'écrire vers l'erreur standard en utilisant la macro ```eprintln!```, qui a la même syntaxe que ```println!``` mais écrit directement vers l'erreur standard.

## Plongée en profondeur

Écrire vers l'erreur standard en Rust peut sembler anodin, mais c'est une fonctionnalité très utile lors du débogage de votre code. Écrire directement vers l'erreur standard vous permet de voir immédiatement les messages d'erreur même si vous n'êtes pas en train de lire la sortie console. Cela peut vous faire gagner du temps lors du débogage de problèmes complexes.

Il est également possible d'écrire d'autres types de données vers l'erreur standard en utilisant la fonction ```write!```. Par exemple, vous pouvez écrire des données d'un tableau ou d'une structure personnalisée directement vers l'erreur standard.

## Voir aussi

- [Documentation officielle de Rust sur l'écriture vers l'erreur standard](https://doc.rust-lang.org/std/io/fn.stderr.html)
- [Guide complet de mise en route avec Rust](https://www.rust-lang.org/learn/get-started)