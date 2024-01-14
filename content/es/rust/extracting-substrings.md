---
title:    "Rust: Extrayendo subcadenas"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una tarea común en la programación, especialmente en situaciones donde se necesita manipular y trabajar con datos de texto. Rust ofrece una forma eficiente y segura de realizar esta tarea, lo que lo convierte en un lenguaje de programación ideal para aquellos que necesitan trabajar con subcadenas de manera regular.

## Cómo hacerlo

Para extraer subcadenas en Rust, utilizaremos el método de la función `get()` en un `str` o una cadena. Este método toma dos parámetros: un índice de inicio y un índice de final, y devuelve una subcadena que comienza en el índice de inicio y termina justo antes del índice de final.

```Rust
let cadena = "Rust es increíble";
let subcadena = cadena.get(5..11);
println!("{}", subcadena); // salida: "es incr"
```

En este ejemplo, estamos extrayendo la subcadena "es incr" de la cadena original "Rust es increíble", comenzando en el índice 5 y terminando justo antes del índice 11.

También podemos utilizar el método `get()` en una cadena literal, sin la necesidad de almacenarla en una variable.

```Rust
let subcadena = "Rust es increíble".get(5..11);
println!("{}", subcadena); // salida: "es incr"
```

Además del método `get()`, también podemos utilizar el operador de corte `[]` para extraer una subcadena de una cadena.

```Rust
let cadena = "Rust es increíble";
let subcadena = &cadena[5..11];
println!("{}", subcadena); // salida: "es incr"
```

## Profundizando

Puede parecer que el método `get()` y el operador de corte `[]` hacen lo mismo, pero en realidad tienen algunas diferencias importantes. Mientras que `get()` devuelve una subcadena opcional, `[]` devuelve una referencia a la subcadena.

Además, `get()` acepta rangos de índices, lo que nos permite extraer varias subcadenas a la vez. También podemos utilizar el método `chars()` para dividir la cadena en caracteres individuales antes de aplicar `get()`.

Otra cosa a tener en cuenta es que los índices de los extremos del rango son inclusivos, lo que significa que el índice inicial está incluido en la subcadena resultante, mientras que el índice final no lo está.

Para obtener más información sobre la manipulación de cadenas en Rust, consulta la documentación oficial y los recursos en línea.

## Ver también

- [Documentación oficial de Rust sobre cadenas](https://doc.rust-lang.org/std/string/)
- [Tutorial de Rust en español](https://rust-lang-es.github.io/)
- [Blog en español sobre Rust](https://www.rust-spanish.com/)