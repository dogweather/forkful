---
title:    "Gleam: Escribiendo pruebas"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué

Escribir pruebas o tests es una parte esencial del proceso de programación. No solo ayuda a detectar y corregir errores en el código, sino que también sirve para asegurar que la funcionalidad del programa se mantenga constante a medida que se realizan cambios en el código. Además, permite tener una mayor confianza en la calidad y estabilidad del software.

## Cómo hacerlo

A continuación, se presentará un breve ejemplo de cómo escribir pruebas en Gleam. En este caso, utilizaremos funciones matemáticas simples para ilustrar el proceso.

Primero, definimos las funciones a ser probadas en nuestro archivo de código:

```Gleam
fn sum(a, b) {
    a + b
}

fn mul(a, b) {
    a * b
}

fn sub(a, b) {
    a - b
}
```

Luego, utilizando el módulo `gleam/test`, podemos realizar las pruebas y verificar si los resultados coinciden con lo esperado. Por ejemplo:

```Gleam
import gleam/test
import . # importamos todas las funciones definidas anteriormente

fn test_sum() {
    describe "Probando función de suma" {
        test "1 + 1 = 2" {
            expect(sum(1, 1)).to_equal(2)
        }

        test "2 + 3 = 5" {
            expect(sum(2, 3)).to_equal(5)
        }

        test "5 + 0 = 5" {
            expect(sum(5, 0)).to_equal(5)
        }
    }
}

fn test_mul() {
    describe "Probando función de multiplicación" {
        test "2 * 4 = 8" {
            expect(mul(2, 4)).to_equal(8)
        }

        test "3 * 0 = 0" {
            expect(mul(3, 0)).to_equal(0)
        }

        test "-2 * 3 = -6" {
            expect(mul(-2, 3)).to_equal(-6)
        }
    }
}

fn test_sub() {
    describe "Probando función de resta" {
        test "10 - 5 = 5" {
            expect(sub(10, 5)).to_equal(5)
        }

        test "5 - 10 = -5" {
            expect(sub(5, 10)).to_equal(-5)
        }

        test "0 - 0 = 0" {
            expect(sub(0, 0)).to_equal(0)
        }
    }
}

fn test_main() {
    describe "Realizando pruebas de funciones matemáticas" {
        test_sum()
        test_mul()
        test_sub()
    }
}

fn suite() {
    test_main()
    test/exit()   // sale del programa si hay alguna prueba fallida
}
```

Este es solo un ejemplo simple, pero se puede aplicar este mismo proceso para probar cualquier tipo de función o módulo en Gleam.

## Profundizando

Al escribir pruebas en Gleam, es importante tener en cuenta algunas recomendaciones. En primer lugar, se deben escribir pruebas que cubran los diferentes casos posibles de entrada y salida. Esto garantizará una cobertura más completa y efectiva.

Además, también es importante utilizar una herramienta de construcción como `gleam/eko` o `rebar3` para automatizar la ejecución de las pruebas y obtener informes detallados sobre su rendimiento.

Por último, es fundamental que las pruebas sean mantenidas y actualizadas a medida que se realizan cambios en el código. Esto asegurará que el software mantenga su calidad y funcionalidad a lo largo del tiempo.

## Ver también

- [Documentación oficial de pruebas en Gleam](https://gleam.run/book/tour/testing.html)
- [Ejemplos de pruebas en Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/tests/tests.gleam)
- [Herramienta de construcción `eko`](https://ekoz.github.io/)
- [Herramienta de construcción `rebar3`](https://github.com/erlang/rebar3)