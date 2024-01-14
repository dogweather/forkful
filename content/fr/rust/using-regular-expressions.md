---
title:                "Rust: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur débutant ou expérimenté, vous avez probablement entendu parler des expressions régulières (ou "regex"). Ces outils puissants sont utilisés pour rechercher et manipuler des chaînes de caractères dans de nombreux langages de programmation, y compris Rust. Alors, pourquoi devriez-vous vous intéresser à l'utilisation des regex en Rust ?

Les expressions régulières peuvent être particulièrement utiles dans les situations suivantes :
- Valider les entrées de l'utilisateur : les regex peuvent vous aider à vérifier si les mots de passe, adresses email ou autres données saisies par l'utilisateur correspondent au format attendu.
- Rechercher et remplacer des chaînes de caractères : vous pouvez utiliser les regex pour trouver des mots ou des motifs spécifiques dans un texte et les remplacer par un autre contenu.
- Extraire des informations d'un texte : si vous avez besoin de récupérer uniquement certaines parties d'une chaîne de caractères, les regex peuvent vous aider à extraire ces données.

En résumé, utiliser les regex en Rust peut vous faire gagner du temps et améliorer l'efficacité de votre code.

## Comment faire

Maintenant que nous avons vu pourquoi utiliser les regex en Rust, passons à la partie pratique. Pour utiliser les regex dans votre code Rust, vous devrez importer le crate `regex`. Voici un exemple de code pour rechercher un schéma de numéro de téléphone dans une chaîne de caractères :

```
use regex::Regex;

fn main() {
    let text = "Mon numéro de téléphone est le 06 12 34 56 78.";
    let re = Regex::new(r"(\d{2} ){4}\d{2}").unwrap();
    let num_telephone = re.find(text).unwrap().as_str();
    println!("{}", num_telephone);
}
```

Dans cet exemple, nous avons importé `Regex` à la ligne 1, créé notre chaîne de caractères à la ligne 3 et défini notre regex à la ligne 4. Le schéma regex que nous recherchons correspond à quatre groupes de deux chiffres suivis de deux chiffres, séparés par un espace. Nous avons utilisé `unwrap()` pour nous assurer que notre regex est valide. Ensuite, à la ligne 5, nous avons utilisé la méthode `find()` pour trouver la première occurrence de ce schéma dans notre chaîne de caractères. Enfin, à la ligne 6, nous avons utilisé `as_str()` pour extraire la partie correspondant à notre schéma et l'afficher.

Vous pouvez également utiliser les regex pour valider les entrées de l'utilisateur ou remplacer certains mots dans une chaîne de caractères. Le crate `regex` fournit une documentation complète avec de nombreux exemples pour vous aider à utiliser au mieux les expressions régulières en Rust.

## Plongée en profondeur

Si vous souhaitez en savoir plus sur les regex en Rust, vous pouvez également consulter la RFC 2363 et l'article [Regular Expressions Cookbook](https://www.joyent.com/blog/regular-expressions-101) pour des informations détaillées sur la syntaxe et les fonctionnalités des regex en Rust.

## Voir aussi

- [Documentation du crate regex](https://docs.rs/regex/)
- [RFC 2363](https://docs.rs/regex/)
- [Regular Expressions Cookbook](https://www.joyent.com/blog/regular-expressions-101)