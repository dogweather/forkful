---
title:    "Rust: Capitaliser une chaîne de caractères"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Pourquoi

Capitaliser une chaîne de caractères peut sembler un petit détail dans la programmation, mais cela peut avoir un grand impact sur la lisibilité et la compréhension du code. En utilisant Rust, nous pouvons capitaliser une chaîne de caractères de manière efficace et élégante, ce qui rendra nos programmes plus clairs et faciles à utiliser pour les autres.

## Comment faire

Pour capitaliser une chaîne de caractères en utilisant Rust, nous pouvons utiliser la fonction `to_uppercase()` qui est disponible dans la bibliothèque standard. Voyons un exemple de code:

```Rust
let mut string = String::from("rust est génial !");
println!("Chaîne originale: {}", string);

string = string.to_uppercase();
println!("Chaîne capitalisée: {}", string);
```

Ce code crée une chaîne de caractères initiale, puis utilise la méthode `to_uppercase()` pour la capitaliser. Le résultat de l'exécution de ce programme sera:

```
Chaîne originale: rust est génial !
Chaîne capitalisée: RUST EST GÉNIAL !
```

Comme vous pouvez le constater, la méthode `to_uppercase()` a converti toute la chaîne en majuscules, ce qui est exactement ce que nous cherchions à faire.

## Plongée en profondeur

Si nous regardons de plus près la méthode `to_uppercase()`, nous pouvons voir qu'elle utilise en interne l'algorithme de l'Unicode pour effectuer la capitalisation. Cela signifie que cette méthode peut fonctionner avec des caractères de n'importe quelle langue qui sont pris en charge par l'Unicode. Cela garantit que notre code est compatible avec toutes les langues et cultures, ce qui est un gros avantage pour un langage de programmation moderne comme Rust.

## Voir aussi

- Documentation officielle de Rust sur la fonction `to_uppercase()`: https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase
- Article sur la manipulation de chaînes de caractères en Rust: https://www.freecodecamp.org/news/how-to-manipulate-strings-in-rust/

Maintenant que vous savez comment capitaliser une chaîne de caractères en utilisant Rust, vous pouvez l'appliquer à vos projets et améliorer la lisibilité de votre code. N'hésitez pas à explorer davantage la documentation et à découvrir d'autres fonctionnalités utiles de Rust pour améliorer votre expérience de programmation. Bon codage!