---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

La concatenación de strings es unir dos o más cadenas de texto en una sola. Los programadores lo hacemos para manipular y presentar datos de manera más eficiente y clara.

## Cómo se hace:

La manera más directa para concatenar strings en Rust es usando el operador `+` o el método `format!()`.

```Rust
fn main() {
    let saludo = "Hola".to_string();
    let nombre = " Pepe".to_string();
    
    let saludo_completo = saludo + &nombre;

    println!("{}", saludo_completo);
}
```

Y este es el resultado:

```Rust
"Hola Pepe"
```

También puedes usar el método `format!()` para concatenar strings de una forma más dinámica y flexible.

```Rust
fn main() {
    let saludo = "Hola";
    let nombre = "Pepe";
    
    let saludo_completo = format!("{} {}", saludo, nombre);

    println!("{}", saludo_completo);
}
```

Y este es el resultado:

```Rust
"Hola Pepe"
```

## Buceo Profundo

La concatenación de strings es un concepto antiguo en programación, con diferentes métodos implementados en distintos lenguajes. En Rust, la eficiencia y la seguridad son prioritarios. En otros lenguajes, concatenar strings puede resultar en problemas de rendimiento, pero Rust maneja esto a través de su sistema de propiedad y tiempo de vida.

Comparado con el operador `+`, `format!()` es más potente pero también consume más recursos. Si concatenas strings grandes o frecuentemente, podrías considerar alternativas como `push_str()` o usar librerías de terceros optimizadas para esta tarea.

Es también importante recordar que en Rust, los strings son UTF-8 por defecto. Esto significa que al concatenar strings podríamos encontrarnos con problemas si nuestros datos no están codificados correctamente.

## Ver También

Para más detalles, consulta la documentación oficial de Rust en [std::string::String](https://doc.rust-lang.org/std/string/struct.String.html) y [std::fmt](https://doc.rust-lang.org/std/fmt/). La sección de Preguntas Frecuentes de Rust también tiene una entrada útil sobre cómo manejar [Strings](https://www.rust-lang.org/es/FAQ.html#how-do-i-concatenate-strings).