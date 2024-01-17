---
title:                "Redacción de pruebas"
html_title:           "Bash: Redacción de pruebas"
simple_title:         "Redacción de pruebas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-tests.md"
---

{{< edit_this_page >}}

¡Hola, programadores de Bash!

## ¿Qué & Por qué?

Escribir pruebas (o tests) es una parte importante de la programación. Se trata de crear pequeños scripts que evalúan si nuestro código funciona correctamente. Los programadores lo hacen para asegurarse de que su código se comporte de la manera esperada y para evitar errores en el futuro.

## Cómo hacerlo:

En Bash, podemos escribir pruebas utilizando el comando `assert`. Por ejemplo, si queremos asegurarnos de que un archivo exista, podemos escribir:

```Bash
assert -e mi_archivo.txt
```

El comando `assert` verificará si el archivo especificado existe y en caso contrario, mostrará un error. También podemos utilizar `assert` para verificar si una variable tiene el valor esperado:

```Bash
nombre="Maria"
assert $nombre == "Maria"
```

Si la variable `nombre` no tiene el valor "Maria", `assert` mostrará un error.

## Profundizando:

Las pruebas en el mundo de la programación no son nada nuevo. De hecho, su uso se remonta a la década de 1950 cuando los programadores necesitaban una manera de verificar su código sin tener que ejecutarlo todo manualmente. En ese entonces, se utilizaban pruebas manuales, lo cual era muy tedioso y propenso a errores.

Hoy en día, existen otras herramientas para escribir pruebas en Bash, como `shunit2` o `bats`. Estas herramientas proporcionan una sintaxis más sencilla y una mejor organización de las pruebas. Sin embargo, el uso de `assert` sigue siendo una forma simple y efectiva de escribir pruebas en Bash.

## Ver también:

Si quieres aprender más sobre el uso de `assert` y otras herramientas de pruebas en Bash, te recomendamos revisar estas fuentes:

- [Documentación oficial de Bash](https://www.gnu.org/software/bash/manual/html_node/Assertions.html)
- [Artículo sobre pruebas en Bash en Opensource.com](https://opensource.com/article/19/4/functional-tests-bash)
- [Tutorial de pruebas en Bash de Tutorialspoint](https://www.tutorialspoint.com/unix/shell_scripting.htm)