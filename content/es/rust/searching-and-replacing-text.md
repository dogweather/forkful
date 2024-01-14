---
title:    "Rust: Buscando y reemplazando texto"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué

Reemplazar texto en un programa es una tarea común durante el proceso de desarrollo y mantenimiento de código. En Rust, esta tarea se puede lograr de manera eficiente y segura utilizando sus poderosas características de manejo de cadenas. En esta publicación del blog, aprenderemos cómo podemos realizar fácilmente operaciones de búsqueda y reemplazo de texto en Rust.

## Cómo hacerlo

El primer paso para realizar una búsqueda y reemplazo de texto en Rust es importar la biblioteca estándar de manejo de cadenas. Esto se puede hacer colocando la línea `use std::str` en la parte superior de su archivo de código. A continuación, podemos utilizar la función `replace()` para reemplazar una subcadena con otra en una cadena dada. Por ejemplo:

```
let text = "Hola mundo!";
let new_text = str::replace(text, "Hola", "Adiós");
println!("{}", new_text);
```

La salida de este código sería `Adiós mundo!`, ya que la subcadena "Hola" se ha reemplazado por "Adiós". La función `replace()` también puede tomar un tercer argumento opcional para especificar la cantidad máxima de reemplazos a realizar. Además, podemos usar `replace_first()` y `replace_last()` para solo reemplazar la primera o la última aparición de la subcadena en una cadena, respectivamente.

Otra forma de realizar una búsqueda y reemplazo de texto en Rust es utilizando expresiones regulares. Esto se puede hacer importando la biblioteca `regex` y utilizando la función `replace_all()` para reemplazar todas las apariciones de una expresión regular con una cadena dada. Por ejemplo:

```
use regex::Regex;
let text = "Hola mundo!";
let re = Regex::new(r"o").unwrap();
let new_text = re.replace_all(text, "a");
println!("{}", new_text);
```

La salida de este código sería `Hala mundo!`, ya que todas las letras "o" han sido reemplazadas por "a" utilizando la expresión regular `r"o"`.

## Profundizando

Rust ofrece una amplia variedad de funciones y tipos de datos relacionados con el manejo de cadenas. Además de los mencionados anteriormente, también podemos utilizar la función `replace_range()` para reemplazar los caracteres en un rango específico de una cadena. También podemos utilizar tipos de cadena específicos, como `String` y `str`, que difieren en el manejo de la propiedad y el rendimiento. Para obtener más información sobre el manejo de cadenas en Rust, puedes consultar la documentación oficial de Rust o explorar otros recursos en línea.

## Ver también

- [Documentación oficial de Rust sobre el manejo de cadenas](https://doc.rust-lang.org/stable/std/str/index.html)
- [Tutorial de Rust: Manejo de cadenas](https://www.tutorialspoint.com/rust/rust_strings.htm)
- [The Rust Programming Language Book - Capítulo 8: Manejo de cadenas](https://doc.rust-lang.org/book/ch08-02-strings.html)