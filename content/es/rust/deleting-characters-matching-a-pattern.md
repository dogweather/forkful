---
title:    "Rust: Eliminar caracteres que coincidan con un patrón"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo tenemos que lidiar con grandes cantidades de datos y texto. En ocasiones, podemos encontrarnos con la necesidad de borrar caracteres de un texto que coincidan con un patrón específico. En este artículo, aprenderemos cómo hacerlo utilizando Rust.

## Cómo hacerlo

Para eliminar caracteres que coincidan con un patrón en Rust, podemos utilizar la función `replace_all()` de la biblioteca `regex`. Esta función toma dos argumentos: el texto original y el patrón a buscar y reemplazar. A continuación, escribiremos un pequeño ejemplo para ilustrar cómo funciona:

```Rust
use regex::Regex;
fn main() {
    let texto = "Esto es un ejemplo de texto con caracteres no deseados. @_@";
    let re = Regex::new("[^a-zA-Z]").unwrap();
    let texto_limipio = re.replace_all(&texto, "");
    println!("{}", texto_limpio);
}
```

En este ejemplo, primero importamos la biblioteca `regex` y luego creamos una instancia de la función `Regex` para especificar el patrón que queremos eliminar, en este caso, cualquier cosa que no sea una letra mayúscula o minúscula. Luego, llamamos a la función `replace_all()` pasando el texto original y el patrón. Finalmente, imprimimos el resultado, que en este caso sería "Estoesunejemplodetextoconcaracteresnodeseados".

## Profundizando

Ahora que hemos visto cómo utilizar la función `replace_all()`, podemos profundizar un poco más en cómo funciona. Esta función utiliza expresiones regulares, también conocidas como regex, para buscar y reemplazar texto. En el ejemplo anterior, utilizamos el patrón `[^a-zA-Z]` para eliminar cualquier cosa que no fuera una letra. Aquí hay algunas expresiones regulares comunes que pueden ser útiles para eliminar caracteres no deseados:

- `[^0-9]` elimina cualquier dígito numérico
- `[^a-z]` elimina cualquier letra minúscula
- `[^A-Z]` elimina cualquier letra mayúscula
- `[^a-zA-Z0-9]` elimina cualquier letra o dígito

Hay muchas más expresiones regulares que pueden ser utilizadas para eliminar caracteres no deseados en un texto. Puedes encontrar más información al respecto en la documentación de `regex`.

## Ver también

- Documentación de `regex`: [https://crates.io/crates/regex](https://crates.io/crates/regex)
- Tutorial de expresiones regulares: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- Documentación de Rust: [https://www.rust-lang.org/es](https://www.rust-lang.org/es)