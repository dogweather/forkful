---
title:    "Rust: Extraction de sous-chaînes"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur en quête de performance et d'efficacité, alors l'extraction de sous-chaînes (substrings) en Rust pourrait être la solution pour vous. Que vous travailliez sur des applications web, des services réseau ou des applications de bureau, Rust offre une approche moderne pour gérer les chaînes de caractères de manière efficace et concise.

## Comment faire

Pour extraire des sous-chaînes en Rust, vous pouvez utiliser la méthode `substring` avec des indices de début et de fin. Par exemple, pour extraire une sous-chaîne de la chaîne `"Bonjour tout le monde"`, vous pouvez utiliser le code suivant :

```Rust
let chaine = "Bonjour tout le monde";
let sous_chaine = chaine.substring(8, 14);
println!("La sous-chaîne est : {}", sous_chaine);
```

Cela produira la sortie suivante :

```
La sous-chaîne est : tout le
```

Vous pouvez également utiliser des méthodes telles que `chars` et `bytes` pour extraire des caractères et des octets spécifiques d'une chaîne.

## Exploration en profondeur

Le fait que les chaînes de caractères en Rust soient des types natifs rend l'extraction de sous-chaînes plus efficace et plus performante que d'autres langages de programmation. De plus, grâce à l'utilisation de la vérification des limites à la compilation, les erreurs comme le débordement de tampon sont évitées, ce qui rend votre code plus sûr et plus robuste.

En outre, l'extraction de sous-chaînes en Rust peut être effectuée à l'aide de différentes méthodes telles que `chars` et `bytes` pour une plus grande flexibilité dans la gestion des chaînes de caractères.

## Voir aussi

- [Documentation sur les chaînes de caractères en Rust](https://doc.rust-lang.org/std/string/index.html)
- [Exemples de manipulation de chaînes de caractères en Rust](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html)
- [Comparaison des chaînes de caractères en Rust et en C++](https://stackoverflow.com/questions/29316755/string-manipulation-performance-comparison-rust-vs-c11)