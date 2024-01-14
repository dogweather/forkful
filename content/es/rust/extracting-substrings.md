---
title:    "Rust: Extrayendo subcadenas"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una habilidad útil en la programación, especialmente en situaciones en las que necesitamos manipular y analizar cadenas de texto de manera eficiente. En este artículo, aprenderemos cómo podemos hacer esto en Rust.

## Cómo hacerlo

Primero, importamos el módulo `str` para tener acceso a las funciones relacionadas con las cadenas de texto. A continuación, podemos usar la función `.slice()` para extraer una parte de una cadena en función de su índice inicial y final. Por ejemplo:

```Rust
let string = "Hola, Mundo!";
let substring = &string[0..4];

println!("La subcadena es: {}", substring);
```

La salida de este código sería:

```
La subcadena es: Hola
```

También podemos usar `.slice()` con índices negativos para contar desde el final de la cadena. Por ejemplo:

```Rust
let string = "Hola, Mundo!";
let substring = &string[..5];

println!("La subcadena es: {}", substring);
```

La salida sería:

```
La subcadena es: Hola,
```

Otra forma de extraer subcadenas es usando la función `.split()` para dividir una cadena en partes basadas en un carácter delimitador. Por ejemplo:

```Rust
let string = "¡Hola|¿Cómo estás?|Bienvenido!";
let substrings = string.split("|");

for substring in substrings {
    println!("La subcadena es: {}", substring);
}
```

La salida de este código sería:

```
La subcadena es: ¡Hola
La subcadena es: ¿Cómo estás?
La subcadena es: Bienvenido!
```

También podemos usar `.split()` con un máximo de secciones a ser divididas. Por ejemplo, si queremos obtener solo las dos primeras partes de la cadena, podemos hacerlo así:

```Rust
let string = "¡Hola|¿Cómo estás?|Bienvenido!";
let substrings = string.splitn(2, "|");

for substring in substrings {
    println!("La subcadena es: {}", substring);
}
```

La salida sería:

```
La subcadena es: ¡Hola
La subcadena es: ¿Cómo estás?|Bienvenido!
```

## Profundizando

Hay más formas de extraer subcadenas en Rust, como usando `.chars()` para obtener una iteración de caracteres, o usando expresiones regulares para buscar patrones específicos en una cadena. También es importante tener en cuenta que las subcadenas en Rust son inmutables, por lo que cualquier modificación en una subcadena afectará a la cadena original.

## Ver También

- [Documentación de Rust sobre cadenas de texto](https://doc.rust-lang.org/std/str/index.html)
- [Guía de Rust sobre expresiones regulares](https://doc.rust-lang.org/std/regex/)
- [Sitio de Rust para encontrar patrones de cadenas](https://crates.io/search?q=strings&sort=newest)
- [Artículo de blog sobre manipulación de cadenas en Rust](https://blog.logrocket.com/how-to-manipulate-strings-in-rust/)