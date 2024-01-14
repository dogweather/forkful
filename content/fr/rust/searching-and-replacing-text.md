---
title:    "Rust: Recherche et remplacement de texte"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur ou développeur en herbe, vous avez probablement déjà utilisé des éditeurs de texte pour effectuer des tâches comme la recherche et le remplacement de texte. Mais saviez-vous qu'il existe une façon plus efficace et plus puissante de le faire en utilisant un langage de programmation moderne comme Rust ? Dans cet article, nous allons explorer pourquoi il est intéressant d'utiliser Rust pour la recherche et le remplacement de texte.

## Comment faire

Tout d'abord, nous avons besoin d'une chaîne de texte pour effectuer nos recherches et remplacements. Voici un exemple de chaîne de texte :

```Rust
let texte = "Bonjour à tous ! Comment ça va ?";
```

Maintenant que nous avons notre texte, voyons comment utiliser les fonctions de recherche et remplacement de Rust :

```Rust
// Recherche du mot "Bonjour" et remplacement par "Salut"
let nouveau_texte = texte.replace("Bonjour", "Salut");

// Recherche de toutes les occurrences de la lettre "o" et remplacement par "a"
let encore_nouveau_texte = texte.replace("o", "a");
```

Dans cet exemple, nous avons utilisé la fonction `replace()` pour effectuer nos recherches et remplacements. Vous pouvez également utiliser des expressions régulières pour des remplacements complexes. Voyons maintenant la sortie de notre code :

```Rust
println!("{}", nouveau_texte);

// Résultat : Salut à tous ! Comment ça va ?

println!("{}", encore_nouveau_texte);

// Résultat : Banjaur à taus ! Cemment ça va ?
```

Comme vous pouvez le voir, la fonction `replace()` nous permet de facilement trouver et remplacer du texte dans une chaîne.

## Plongeons plus en profondeur

Maintenant que nous savons comment utiliser les fonctions de recherche et remplacement en Rust, voyons quelques aspects plus avancés de ces fonctions. Il est important de noter que ces fonctions retournent une nouvelle chaîne après avoir effectué le remplacement, ce qui signifie que les chaînes originales ne sont pas modifiées.

De plus, la fonction `replace()` accepte également un troisième argument qui permet de limiter le nombre de remplacements effectués. Voyons un exemple :

```Rust
// Limite le nombre de remplacements à 2
let limite = texte.replace("a", "b", 2);

println!("{}", limite);

// Résultat : Bonbjour à tous ! Comment ça va ?
```

Vous pouvez également utiliser la fonction `replace_range()` si vous voulez remplacer un certain intervalle de caractères dans une chaîne. Voici un exemple :

```Rust
// Remplace les caractères de la position 3 à 7 par "qu"
let nouveau = texte.replace_range(3..7, "qu");

println!("{}", nouveau);

// Résultat : Bonqu à tous ! Comment ça va ?
```

## Voir aussi

Si vous souhaitez en savoir plus sur la recherche et le remplacement en Rust, voici quelques ressources utiles :

- [La documentation officielle de Rust sur les chaînes de caractères](https://doc.rust-lang.org/std/string/struct.String.html)
- [Un tutoriel sur la manipulation de chaînes de caractères en Rust](https://danielkeep.github.io/tlborm/book/pat-chars.html)
- [Un article sur la manipulation de chaînes de caractères avec les expressions régulières en Rust](https://blog.burntsushi.net/transducers/)

Maintenant que vous êtes armé de ces connaissances, n'hésitez pas à expérimenter et à utiliser le pouvoir de Rust pour vos futures tâches de recherche et remplacement de texte !