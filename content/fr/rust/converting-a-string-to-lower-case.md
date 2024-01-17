---
title:                "Convertir une chaîne de caractères en minuscules"
html_title:           "Rust: Convertir une chaîne de caractères en minuscules"
simple_title:         "Convertir une chaîne de caractères en minuscules"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi ?
La conversion d'une chaîne de caractères en lettres minuscules signifie simplement transformer toutes les lettres majuscules en lettres minuscules. Les programmeurs font souvent cela pour normaliser les données ou pour faciliter les comparaisons entre chaînes de caractères.

# Comment faire :
Voici un exemple de code en Rust pour convertir une chaîne de caractères en lettres minuscules :

```Rust
let string = "Bonjour, MONDE !";
let string_minuscule = string.to_lowercase();
println!("{}", string_minuscule);
```

La sortie de ce code serait "bonjour, monde !". Comme vous pouvez le voir, toutes les lettres majuscules ont été transformées en lettres minuscules.

# Plongée en profondeur :
Historiquement, cette fonctionnalité n'était pas disponible dans les langages de programmation, mais elle est devenue très utile avec l'essor des logiciels multilingues. En plus de la méthode présentée ci-dessus, il existe également des bibliothèques externes qui peuvent gérer la conversion de manière différente, comme en prenant en compte les accents.

# Voir aussi :
Pour plus d'informations sur la conversion de chaînes de caractères en lettres minuscules en Rust, vous pouvez consulter la documentation officielle sur [le type de données String](https://doc.rust-lang.org/1.28.0/std/string/struct.String.html) ou jeter un coup d'œil aux différentes bibliothèques externes telles que [strcase](https://crates.io/crates/strcase) ou [unicase](https://crates.io/crates/unicase).