---
title:    "Rust: Convertir une chaîne en minuscule"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Pourquoi

Il peut y avoir différentes raisons pour lesquelles vous souhaiteriez convertir une chaîne de caractères en minuscules. Peut-être que vous voulez normaliser les données saisies par l'utilisateur pour une comparaison plus facile ou peut-être que vous devez traiter une chaîne de caractères provenant d'une source externe qui peut être en majuscules ou en minuscules. Quelle que soit la raison, savoir comment convertir une chaîne de caractères en minuscules peut être une compétence utile en programmation Rust.

# Comment Faire

Pour convertir une chaîne de caractères en minuscules en utilisant Rust, il suffit d'utiliser la méthode ```to_lowercase()```. Cette méthode prend un object ```str``` et renvoie un nouveau ```String``` contenant la même chaîne de caractères, mais en minuscules.

Voici un exemple de code pour illustrer comment utiliser cette méthode:

```Rust
let string = "RUST";
let lower_case = string.to_lowercase();
println!("{}", lower_case); // Output: rust
```

# Plongée Profonde

Lorsque vous utilisez la méthode ```to_lowercase()```, il est important de comprendre comment elle fonctionne réellement. En fait, cette méthode utilise des règles de casse définies dans l'implémentation Unicode. Ces règles sont utilisées pour déterminer quelles lettres doivent être converties en minuscules et comment elles doivent être converties.

De plus, il est important de noter que la méthode ```to_lowercase()``` renvoie toujours un nouveau ```String```, même si la chaîne d'origine est déjà en minuscules. Cela signifie que si vous appelez cette méthode sur une chaîne qui est déjà en minuscules, vous obtiendrez une copie identique de la chaîne.

# Voir Aussi

Pour plus d'informations sur les méthodes de manipulation de chaînes de caractères en Rust, consultez les liens suivants:

- [Guide officiel de Rust sur les chaînes de caractères](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Documentation officielle de la méthode to_lowercase()](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)