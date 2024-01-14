---
title:    "Bash: Escribiendo pruebas"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Por Qué

Escribir pruebas en Bash puede parecer un paso adicional y tedioso al principio, pero en realidad es una parte esencial del proceso de programación. Las pruebas nos permiten validar rápidamente nuestro código y detectar posibles errores antes de que se conviertan en problemas mayores en producción. Además, escribir pruebas nos ayuda a garantizar que nuestro código sea robusto y confiable.

## Cómo Hacerlo

Para escribir pruebas en Bash, utilizamos el comando `test` que nos permite evaluar expresiones y tomar decisiones basadas en el resultado. Veamos un ejemplo sencillo de una prueba que comprueba si un número es mayor que 10:

```Bash
#!/bin/bash
# Definimos una variable con un número
num=15
# Utilizamos el comando `test` para evaluar la expresión
if test $num -gt 10; then
    echo "El número es mayor que 10"
fi
```

En este caso, utilizamos la opción `-gt` que significa "greater than" (mayor que), pero también podemos utilizar otras opciones como `-lt` (menor que), `-eq` (igual a), entre otras.

También podemos utilizar el comando `test` para comparar cadenas de texto. Por ejemplo, si queremos comprobar si una variable contiene el nombre "Maria", podemos hacerlo de la siguiente manera:

```Bash
#!/bin/bash
# Definimos una variable con un nombre
nombre="Maria"
# Utilizamos el comando `test` y la opción `-z` para comprobar si la cadena está vacía
if test -z $nombre; then
    echo "La variable está vacía"
else
    echo "Hola $nombre"
fi
```

En este caso, utilizamos la opción `-z` que significa "zero" (cero) y nos permite comprobar si la cadena está vacía.

## Profundizando

Aunque hemos visto ejemplos sencillos, hay muchas otras opciones y casos de uso para escribir pruebas en Bash. Además de las opciones que ya hemos visto, también podemos utilizar operadores lógicos como `&&` (and) y `||` (or) para combinar diferentes expresiones en una sola prueba.

También podemos utilizar el comando `test` junto con otras herramientas de Bash como `grep` y `sed` para realizar pruebas más complejas. De hecho, existen diferentes librerías y frameworks como [bats](https://github.com/sstephenson/bats) que nos permiten escribir pruebas más estructuradas y completas en Bash.

Otra ventaja de escribir pruebas en Bash es que podemos integrarlas en herramientas de integración continua (CI) como [Travis CI](https://travis-ci.com/) para que se ejecuten automáticamente cada vez que hacemos cambios en nuestro código.

## Ver también

- [La Biblia de Bash: Incluye Ejemplos Prácticos y Un Ejemplo de Script Mojo de Sistema](https://www.bashbible.com/)
- [Introducción a los Test Unitarios en Bash](https://medium.com/@pablormier/testing-unitario-en-bash-66c9a959aa31)
- [Cómo Escribir Test Unitarios en Shell Script](https://dev.to/thiht/shell-scripts-testing-made-easy-2h7g)

---

¡Espero que este artículo te haya sido útil para aprender cómo escribir pruebas en Bash! ¡Recuerda siempre incluir pruebas en tu proceso de programación para garantizar un código más sólido y confiable!