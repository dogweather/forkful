---
date: 2024-01-20 17:39:05.509124-07:00
description: "Convertir une cha\xEEne de caract\xE8res en minuscules, \xE7a veut dire\
  \ transformer tous les caract\xE8res majuscules en leur \xE9quivalents en bas de\
  \ casse. On le fait\u2026"
lastmod: '2024-03-13T22:44:57.468321-06:00'
model: gpt-4-1106-preview
summary: "Convertir une cha\xEEne de caract\xE8res en minuscules, \xE7a veut dire\
  \ transformer tous les caract\xE8res majuscules en leur \xE9quivalents en bas de\
  \ casse. On le fait\u2026"
title: "Conversion d'une cha\xEEne de caract\xE8res en minuscules"
weight: 4
---

## Quoi et Pourquoi ?
Convertir une chaîne de caractères en minuscules, ça veut dire transformer tous les caractères majuscules en leur équivalents en bas de casse. On le fait pour uniformiser les données, par exemple pour comparer des entrées utilisateurs sans se soucier de la casse.

## Comment faire :
```Rust
fn main() {
    let message = "Rust, C'EST génIAL!";
    let message_en_minuscules = message.to_lowercase();
    println!("{}", message_en_minuscules);
}
```
Sortie :
```
rust, c'est génial!
```

## Exploration :
Historiquement, transformer des textes en minuscules est une pratique courante, surtout pour les recherches insensibles à la casse et le tri de données textuelles. En Rust, `.to_lowercase()` fait plus que de changer la casse des lettres ASCII. Il s'occupe correctement des lettres accentuées et des caractères spéciaux, grâce à l'implémentation d'Unicode.

Il existe des alternatives, comme `.to_ascii_lowercase()` pour les chaînes constituées uniquement de caractères ASCII, mais cette méthode est moins universelle et ne doit être utilisée que lorsque vous êtes sûrs du contenu de la chaîne.

Le processus de conversion prend en compte les règles de casse complexe d'Unicode, y compris pour des langues comme le turc où la conversion de casse ne suit pas toujours les règles anglaises standard.

## Voir aussi :
- Documentation Rust sur les Strings : https://doc.rust-lang.org/std/string/struct.String.html
- Unicode: https://unicode.org
- Rust by Example, traitement des strings : https://doc.rust-lang.org/rust-by-example/std/str.html
