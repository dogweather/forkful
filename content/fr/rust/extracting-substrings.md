---
title:    "Rust: Extraction de sous-chaînes"
keywords: ["Rust"]
---

{{< edit_this_page >}}

#Pourquoi extraire des sous-chaînes en Rust?

Il est souvent nécessaire de travailler avec des chaînes de caractères dans un programme en Rust. Cela peut impliquer la manipulation de sous-chaînes, c'est-à-dire des parties d'une chaîne plus grande. Cela peut être utile pour analyser des données, filtrer des informations ou encore effectuer des opérations sur des nombres contenus dans une chaîne. Dans cet article, nous allons explorer pourquoi et comment extraire des sous-chaînes en Rust.

##Comment faire

Pour extraire une sous-chaîne en Rust, nous allons utiliser la méthode `.get()` sur une chaîne de caractères. Cette méthode prend en paramètres les indices de début et de fin de la sous-chaîne que nous voulons extraire. Nous pouvons également utiliser la méthode `.char_indices()` pour obtenir les indices de chaque caractère de la chaîne, ce qui peut être utile pour déterminer les indices de début et de fin souhaités.

```
let chaine = "Bienvenue en Rust!";
let sous_chaine = chaine.get(11..,:);
println!("{}", sous_chaine); // output: Rust!
```

Dans cet exemple, nous extrayons la sous-chaîne à partir de l'indice 11 (caractère "R") jusqu'à la fin de la chaîne. Nous utilisons également la méthode `println!()` pour afficher la sous-chaine à l'écran.

##Approfondissement

Il est important de noter que les indices de début et de fin utilisés par la méthode `.get()` sont inclusifs, ce qui signifie que le caractère à l'indice de fin sera également inclus dans la sous-chaîne résultante. De plus, il est recommandé d'utiliser les méthodes `.is_char_boundary()` et `.len()` pour s'assurer que les indices que nous passons sont valides et ne coupent pas des caractères.

Voici un autre exemple d'extraction de sous-chaîne en spécifiant les indices de manière plus précise :

```
let chaine = "Bonjour à tous !";
let indices = chaine.char_indices();
let (mut debut, mut fin) = (0, 0);
for (i, c) in indices {
    if c == 'à' {
        debut = i;
    } else if c == '!' {
        fin = i;
    }
}
let sous_chaine = chaine.get(debut..fin+1);
println!("{}", sous_chaine); // output: à tous !
```

Dans cet exemple, nous parcourons tous les indices de la chaîne jusqu'à trouver le caractère "à" et le caractère "!". Nous utilisons ensuite ces indices pour extraire la sous-chaîne souhaitée.

##Voir aussi

- [La documentation sur les méthodes de chaînes en Rust](https://doc.rust-lang.org/std/string/struct.String.html)
- [Exemples de manipulation de chaînes en Rust](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html)
- [Un tutoriel sur les méthodes de chaînes en Rust](https://www.tutorialspoint.com/rust/rust_strings.htm)