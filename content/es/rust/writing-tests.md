---
title:    "Rust: Escribir pruebas"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Por Qué

La escritura de pruebas es una parte crucial del proceso de programación en Rust. Ayuda a garantizar que nuestro código sea robusto y confiable. Además, nos permite identificar errores y problemas potenciales antes de que se conviertan en problemas en la producción.

## Cómo

Para escribir pruebas en Rust, primero debemos importar el módulo "prueba" usando la palabra clave `use`. Luego, podemos usar el atributo `#[test]` para indicar que una función es una prueba.

```
Rust
use prueba::assert_eq;
 
#[test]
fn suma_debería_ser_correcta() {
    let resultado = suma(2, 3);
    assert_eq!(resultado, 5);
}
```

En el código anterior, estamos probando la función `suma`, que debería devolver la suma de dos números. Usamos la macro `assert_eq` para comparar el resultado de la función con el valor esperado. Si son iguales, la prueba se pasará. De lo contrario, fallará y nos indicará que algo no está funcionando como se espera.

También podemos usar la macro `assert!` para verificar una condición específica en una prueba, como por ejemplo:

```
Rust
assert!(precio_total > 0, "El precio total debe ser mayor que cero");
```

Por último, es importante asegurarse de que nuestras pruebas sean deterministas, es decir, que siempre produzcan el mismo resultado. Para lograr esto, podemos usar la función `seed` del módulo `rand` para generar valores aleatorios controlados.

## Deep Dive

Escribir pruebas efectivas no se trata solo de verificar el correcto funcionamiento de nuestro código, sino también de identificar posibles casos de borde y asegurarse de que nuestro código sea escalable y mantenible en el futuro.

Algunas buenas prácticas para escribir pruebas en Rust incluyen:

- Nombrar adecuadamente nuestras pruebas para que sea fácil de entender qué función se está probando.
- Usar casos de prueba para cubrir diferentes escenarios y casos de borde.
- Revisar el código en busca de posibles problemas de lógica o errores de sintaxis.
- Actualizar y agregar pruebas a medida que el código evoluciona.

## Ver También

- Documentación oficial de Rust sobre escritura de pruebas: https://doc.rust-lang.org/book/ch11-02-running-tests.html
- Ejemplos de pruebas en Rust: https://github.com/rust-lang/rust/tree/master/src/test/ui
- Librería de pruebas TDD en Rust: https://github.com/rust-lang-nursery/tokio-test