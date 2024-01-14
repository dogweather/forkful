---
title:    "Rust: Recherche et remplacement de texte"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Pourquoi
La recherche et le remplacement de texte est une tâche courante dans la programmation et peut s'avérer fastidieuse surtout lorsque vous travaillez sur de grands fichiers. Heureusement, Rust propose des fonctionnalités qui facilitent cette tâche et garantissent en même temps des performances élevées.

## Comment faire
Pour effectuer une recherche et un remplacement de texte en Rust, vous pouvez utiliser la fonction `replace` de la bibliothèque standard `std::string`. Cette fonction prend en paramètres une chaîne de caractères, le texte à rechercher et le texte de remplacement. Voici un exemple de code montrant comment utiliser cette fonction :

```Rust
use std::string;

let mut texte = String::from("Hello world!");
texte.replace("world", "Rust"); // le texte devient "Hello Rust!"
```

Vous pouvez également utiliser des expressions régulières avec la bibliothèque `regex` pour des cas plus complexes. Voici un exemple de code utilisant cette bibliothèque :

```Rust
use regex::Regex;

let mut texte = String::from("Bonjour, je suis un programme en Rust!");
let reg = Regex::new(r"[^\w\s]").unwrap(); // recherche tous les caractères spéciaux
texte = reg.replace_all(texte, "").to_string(); // le texte devient "Bonjour je suis un programme en Rust"
```

Ces exemples sont basiques mais vous pouvez les adapter à vos besoins et combiner différentes fonctions pour obtenir le résultat souhaité. N'hésitez pas à consulter la documentation complète pour en savoir plus sur les différentes options disponibles.

## Plongée en profondeur
La fonction `replace` de la bibliothèque standard est optimisée pour les grandes chaînes de caractères, ce qui garantit des performances élevées même sur de grands fichiers. De plus, la bibliothèque `regex` prend en charge la recherche et le remplacement dans les chaînes de caractères Unicode, offrant ainsi une grande flexibilité.

Il est également important de noter que la fonction `replace` effectue le remplacement directement sur la chaîne d'origine sans créer de nouvelle chaîne, ce qui peut être utile si vous travaillez avec des fichiers volumineux.

## Voir aussi
- [Documentation sur la fonction replace](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Documentation sur la bibliothèque regex](https://docs.rs/regex/1.4.2/regex/)
- [Article sur la manipulation de chaînes de caractères en Rust](https://www.steadylearner.com/blog/read/Rust-Strings-Tutorial)