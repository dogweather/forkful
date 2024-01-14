---
title:    "Rust: Convirtiendo una cadena a minúsculas"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué
En Rust, hay muchas formas de manipular y trabajar con cadenas de texto. Una de estas formas útiles es convertir una cadena de texto a minúsculas. Esto puede ser útil para comparar cadenas de texto sin importar si están en mayúsculas o minúsculas, o simplemente para dar formato a la salida de datos.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Rust, podemos utilizar el método `to_lowercase()`. Este método toma una cadena de texto y devuelve una nueva cadena de texto con todas las letras en minúsculas. Veamos un ejemplo:

```Rust
let nombre = "JORGE";

let nombre_en_min = nombre.to_lowercase();

println!("Mi nombre en minúsculas es {}", nombre_en_min);
```
Este código producirá la siguiente salida:

```
Mi nombre en minúsculas es jorge
```

Podemos ver que la cadena de texto "JORGE" se ha convertido a "jorge" después de pasarla por el método `to_lowercase()`.

## Profundizando
Sin embargo, debemos tener en cuenta que esta conversión a minúsculas no es universal. Depende del idioma y del sistema operativo en el que se ejecuta nuestro código. Por ejemplo, en Windows, una cadena de texto como "ß" puede ser convertida a "ss" en lugar de "ß". Esto se debe a que en alemán, la letra "ß" se representa como "ss" en palabras en mayúsculas.

Además, este método sólo cambia las letras en minúsculas y no afecta a otros caracteres, como símbolos o números. Por lo tanto, si queremos asegurarnos de tener una cadena de texto completamente en minúsculas, es posible que tengamos que combinar este método con otros, como `replace()`.

## Vea también

Aquí hay algunos recursos adicionales sobre el tema que pueden ser útiles:

- [Documentación oficial de Rust sobre el método to_lowercase()](https://doc.rust-lang.org/std/string/trait.ToString.html#tymethod.to_lowercase)
- [Código de ejemplo en Rust Playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=25d72b899de7c20555b8cc31a89d5f00)

¡Esperamos que este artículo te haya sido útil y puedas empezar a convertir cadenas de texto a minúsculas en tus proyectos de Rust!