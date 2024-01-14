---
title:    "Bash: Escribiendo pruebas"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en Bash?

Es común que los programadores dediquen mucho tiempo al desarrollo del código en sí, sin prestar suficiente atención a la escritura de pruebas. Sin embargo, escribir pruebas es una práctica crucial en el proceso de desarrollo de software ya que ayuda a garantizar que el código funcione correctamente y evita errores en producción. Además, puede ahorrar tiempo a largo plazo al facilitar la detección y solución de errores.

## Cómo hacerlo

Para escribir pruebas en Bash, podemos utilizar el comando `assert` que nos permite comparar valores esperados con los resultados obtenidos. Por ejemplo:

```Bash
# Definir una función que devuelva el doble de un número
function doble {
    echo $(($1 * 2))
}

# Prueba utilizando el comando assert
assert "doble 5" 10 "La función debe devolver el doble de un número"
```

En este ejemplo, definimos una función llamada `doble` que devuelve el doble del número pasado como argumento. Luego, usamos el comando `assert` para comparar el resultado de llamar a la función con el número 5, con el valor esperado de 10. Si la prueba falla, se mostrará un mensaje indicando que la función no está devolviendo el valor esperado.

Además de usar el comando `assert`, también podemos hacer uso de la sentencia `if` en nuestro código para realizar diferentes acciones dependiendo de si una prueba es exitosa o falla.

## Profundizando en las pruebas

Al escribir pruebas en Bash, es importante tener en cuenta algunos consejos para asegurarnos de que sean efectivas:

- Sigue un estándar de nomenclatura claro y consistente para tus pruebas.
- Asegúrate de incluir pruebas para todos los posibles casos en tu código.
- Utiliza un marco de pruebas como `shUnit2` para estructurar mejor tus pruebas.
- No subestimes la importancia de las pruebas unitarias y de integración.

En resumen, escribir pruebas en Bash es crucial para garantizar la calidad de nuestro código y evitar errores costosos en producción. Con el uso de herramientas y buenas prácticas, podemos hacer que nuestro proceso de desarrollo sea más eficiente y confiable.

## Ver también

- [Documentación del comando `assert`](https://www.gnu.org/software/shellutils/manual/html_node/Bourne-Shell-Builtins.html)
- [Tutorial de shUnit2](https://www.neotitans.net/shunit2-sencillo-y-eficaz-framework-unit-testing-bash.html)
- [Bash Unit - herramienta para escribir pruebas en Bash](https://github.com/rlancaste/bash_unit)