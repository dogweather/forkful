---
title:                "Escribiendo pruebas"
html_title:           "Rust: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en Rust?

Escribir pruebas en Rust es esencial para garantizar que nuestro código funcione correctamente y para prevenir futuros errores. Además, es una buena práctica de programación que puede ahorrarnos tiempo y esfuerzo a largo plazo.

## Cómo escribir pruebas en Rust

Para empezar, debemos asegurarnos de que nuestro proyecto tenga la estructura adecuada para incluir pruebas. En la raíz del proyecto, deberíamos tener un directorio llamado "tests" donde colocaremos nuestros archivos de pruebas.

Para crear una función de prueba en Rust, simplemente agregamos el atributo `test` encima de la función. Por ejemplo:

```Rust
#[test]
fn test_sum() {
  let result = sum(2, 3);
  assert_eq!(result, 5);
}
```

En el código anterior, hemos creado una función de prueba llamada `test_sum` y hemos utilizado la macro `assert_eq` para verificar que el resultado de nuestra función `sum` sea igual a 5. Si el resultado es diferente, la prueba fallará y obtendremos un mensaje de error.

Podemos ejecutar nuestras pruebas escribiendo `cargo test` en la terminal de nuestro proyecto. Esto ejecutará todas las pruebas en nuestro directorio "tests" y nos mostrará un resumen de los resultados.

## Profundizando en la escritura de pruebas en Rust

Además de la macro `assert_eq`, Rust también tiene otras macros de aserción como `assert!` y `assert_ne!`, que nos permiten verificar diferentes condiciones en nuestras pruebas. También podemos agrupar nuestras pruebas en módulos para una organización más clara y utilizar la macro `#[ignore]` para omitir pruebas específicas.

Es importante recordar que nuestras pruebas también son parte del código y deben seguir las mismas convenciones de estilo y buenas prácticas. Además, es recomendable escribir pruebas para todos los casos de borde y escenarios posibles para garantizar la robustez de nuestro código.

## Ver también

- [Documentación de Rust sobre pruebas y aserciones](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Tutorial de testing en Rust](https://rust-cli.github.io/book/tutorial/testing.html)
- [Ejemplos de pruebas en Rust](https://github.com/rust-lang/rust/blob/master/src/test/run-pass)