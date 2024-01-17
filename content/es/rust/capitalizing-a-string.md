---
title:                "Mayúsculas en una cadena"
html_title:           "Rust: Mayúsculas en una cadena"
simple_title:         "Mayúsculas en una cadena"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por qué y para qué hacer mayúsculas en una cadena

Cuando se habla de capitalizar una cadena en programación, nos referimos a cambiar todas las letras a mayúsculas. Esto puede ser útil en situaciones en las que se necesita comparar cadenas sin importar si tienen mayúsculas o minúsculas, o simplemente para mejorar la legibilidad de una cadena.

Los programadores a menudo capitalizan cadenas para estandarizar el formato o para que una búsqueda o comparación sea más fácil. También puede ser necesario en algunas situaciones de procesamiento de texto.

## Cómo hacerlo:

En Rust, para capitalizar una cadena podemos utilizar el método ```to_uppercase()``` que convierte la cadena a mayúsculas. Por ejemplo:

```Rust
let ejemplo = "texto a capitalizar";
print!("{}", ejemplo.to_uppercase());
```

Este código producirá la salida "TEXTO A CAPITALIZAR".

## Una mirada más profunda:

En el pasado, capitalizar cadenas era especialmente importante en lenguajes que no distinguían entre mayúsculas y minúsculas, como COBOL. En la actualidad, muchos lenguajes de programación cuentan con métodos integrados que permiten capitalizar cadenas de manera sencilla.

Otra alternativa a la hora de capitalizar cadenas es utilizar expresiones regulares, que permiten buscar y reemplazar patrones en una cadena. Sin embargo, esto puede ser más complejo y requerir un mayor conocimiento técnico.

En Rust, la implementación del método ```to_uppercase()``` se basa en Unicode, lo que significa que funcionará con caracteres de cualquier idioma.

## Véase también:

- La documentación oficial de Rust sobre el método ```to_uppercase()``` (https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase)
- Un tutorial sobre cómo utilizar expresiones regulares en Rust (https://blog.burntsushi.net/transducers/)