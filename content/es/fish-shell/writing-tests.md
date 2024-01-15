---
title:                "Escribiendo pruebas"
html_title:           "Fish Shell: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en Fish Shell?

Escribir pruebas en Fish Shell es una forma eficaz de garantizar que nuestro código funcione correctamente y se mantenga libre de errores. Además, nos ayuda a detectar posibles problemas y a mejorar la calidad de nuestro código.

## Cómo hacerlo

```Fish Shell
function sum(number1, number2)
    echo $number1 + $number2
end

test "Sum function should return the correct result" -e (sum 2 3) 5
```

Para escribir pruebas en Fish Shell, podemos utilizar la función `test`, que nos permite verificar si un resultado es el esperado. También podemos crear funciones específicas para realizar pruebas en nuestro código, como en el ejemplo anterior.

La sintaxis de `test` es la siguiente:

```Fish Shell
test <mensaje> <comando que debe ejecutarse> <resultado esperado>
```

Donde:

- `<mensaje>`: Es una descripción del test que estamos realizando.
- `<comando que debe ejecutarse>`: Es el comando (o función) que queremos probar.
- `<resultado esperado>`: Es el resultado que esperamos obtener.

## Profundizando en las pruebas en Fish Shell

Además de la función `test`, también podemos utilizar los parámetros `-e` y `-x` para realizar pruebas más precisas. El parámetro `-e` nos permite verificar si el resultado es igual al esperado, mientras que `-x` nos permite verificar si el resultado es diferente al esperado.

Por ejemplo:

```Fish Shell
test "Sum function should return the correct result" -e (sum 2 3) 5
test "Sum function should not return the incorrect result" -x (sum 2 3) 8
```

En caso de que un test falle, Fish Shell nos mostrará un mensaje de error indicando cuál fue el resultado obtenido y cuál era el esperado.

## Ver también

- Documentación oficial de Fish Shell sobre pruebas: https://fishshell.com/docs/current/cmds/test.html
- Ejemplos de pruebas en Fish Shell: https://github.com/fish-shell/fish-shell/tree/master/test
- Artículo sobre cómo escribir pruebas en Fish Shell: https://dev.to/yeurch/write-some-fish-shell-tests-376e