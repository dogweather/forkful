---
title:    "Rust: Convertir une date en chaîne de caractères."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur en Rust, vous savez peut-être que la manipulation des dates peut être une tâche fastidieuse. Ce n'est pas toujours facile de travailler avec différents formats de date et de les afficher correctement. C'est pourquoi il est important de savoir comment convertir une date en chaîne de caractères en Rust.

## Comment faire

Pour commencer, voici un exemple de code pour convertir une date en chaîne de caractères en utilisant le crate `chrono` :

```Rust
use chrono::{DateTime, Local, FixedOffset};
let now: DateTime<Local> = Local::now();
let offset: FixedOffset = now.offset();
let date_string = now.format("%d/%m/%Y").to_string();
println!("{}", date_string);
```

Lorsque vous exécutez ce code, vous obtiendrez la date actuelle au format jour/mois/année. Dans cet exemple, nous utilisons le format "day/month/year" mais il existe de nombreux autres formats que vous pouvez utiliser en fonction de vos besoins. Vous pouvez consulter la documentation du crate `chrono` pour découvrir toutes les options disponibles.

## Plongée en profondeur

En plongeant plus en profondeur, vous pouvez vous demander comment exactement la conversion de date en chaîne de caractères fonctionne en Rust. Eh bien, cela se fait en utilisant la méthode `to_string` sur l'objet `DateTime` et en spécifiant le format souhaité. La macro `format!` est utilisée pour spécifier le format et les résultats sont renvoyés sous forme de `String`.

De plus, il est important de noter que lors de la conversion en chaîne de caractères, la date est affichée selon la timezone locale de l'utilisateur. Si vous souhaitez spécifier une timezone spécifique, comme dans l'exemple ci-dessus avec `Local`, vous devrez utiliser la méthode `with_timezone` sur l'objet `DateTime`.

## Voir aussi

- La documentation officielle du crate `chrono` : https://docs.rs/chrono/0.4.19/chrono/struct.DateTime.html
- Un exemple de conversion de date en chaîne de caractères avec différentes timezones : https://stackoverflow.com/questions/53445084/rust-how-to-get-datetime-in-a-custom-timezone