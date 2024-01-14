---
title:    "Rust: Utiliser les expressions régulières"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant utilisé pour trouver et manipuler des motifs dans les chaînes de caractères. En programmant en Rust, il est important de comprendre comment utiliser les expressions régulières pour traiter efficacement les données.

## Comment faire

Les expressions régulières sont utilisées en Rust en important le module "regex". Par exemple, si vous souhaitez trouver toutes les occurrences d'un nombre à deux chiffres dans une chaîne de caractères, vous pouvez utiliser la fonction "Regex::new" pour créer une expression régulière et utiliser la méthode "find_iter" pour récupérer les correspondances. Voici un exemple de code:

```Rust
let re = Regex::new(r"\d{2}").unwrap();
let text = "Il y a 42 chats sur le mur.";
for mat in re.find_iter(text) {
    println!("Trouvé '{}'", mat.as_str());
}
```
La sortie de ce code sera "Trouvé '42'".

## Plongée en profondeur

Les expressions régulières peuvent sembler compliquées au premier abord, mais elles suivent des règles de syntaxe spécifiques qui peuvent être apprises et maîtrisées avec de la pratique. Certaines astuces utiles incluent l'utilisation de caractères spéciaux comme les parenthèses pour capturer des groupes de correspondances, et l'utilisation des opérateurs "?" et "*" pour rendre certains motifs optionnels ou répétables. Il est également important de noter que les expressions régulières peuvent être sensibles à la casse et qu'il existe des drapeaux pour les instructions "insensible à la casse" et "multi-lignes".

## Voir aussi

- [Documentation sur les expressions régulières en Rust](https://doc.rust-lang.org/regex/regex/)
- [Guide de référence rapide pour les expressions régulières en Rust](https://faimaison.net/wiki/index.php/Regular_expression_quick_ref_-_Rust)
- [Livres et tutoriels pour apprendre les expressions régulières](https://www.regular-expressions.info/tutorial.html)