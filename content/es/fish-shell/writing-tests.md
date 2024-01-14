---
title:    "Fish Shell: Programando pruebas"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en Fish Shell?

Escribir pruebas en Fish Shell puede ahorrar tiempo y evitar errores en el código. Al tener pruebas automatizadas, se pueden encontrar y corregir errores de forma más rápida y eficiente, garantizando un código más sólido y confiable.

## Cómo hacerlo:

Para escribir pruebas en Fish Shell, se pueden seguir los siguientes pasos:

1. Primero, se debe crear un nuevo archivo con la extensión .fish y guardarlo en la misma carpeta que el código que se desea probar.
2. Se debe importar la biblioteca de pruebas de Fish Shell utilizando el comando `source ./test.fish` al principio del archivo.
3. Se pueden utilizar las funciones `test`, `match`, `contains`, entre otras, para realizar diferentes tipos de pruebas en el código.
4. Finalmente, se debe utilizar la función `contains` para verificar que el código retorna el resultado esperado.

Un ejemplo de código con pruebas sería el siguiente:

```Fish Shell
source ./test.fish

function suma -a num1 num2
  if test "$num1" -eq 0
    return 0
  end

  return (math "$num1" + "$num2")
end

function test_suma
  match (suma 5 5) 10 "La suma de 5 y 5 debe ser 10"
  match (suma 3 7) 11 "La suma de 3 y 7 debe ser 11"
  match (suma 0 0) 0 "La suma de 0 y 0 debe ser 0"
end

test_suma
```

El código anterior define una función `suma` que recibe dos números y los suma, y una función `test_suma` que realiza pruebas utilizando la función `match` para comprobar que el resultado sea el esperado. Al ejecutar este archivo, si todas las pruebas pasan, no se mostrará ninguna salida. De lo contrario, se mostrarán errores en las pruebas que fallaron.

## Profundizando en las pruebas:

Escribir pruebas puede ser beneficioso para detectar errores en el código, además de ayudar a mejorar la calidad y mantener la integridad del código en un proyecto. Algunas recomendaciones para escribir pruebas en Fish Shell incluyen:

- Realizar pruebas para diferentes casos y situaciones, especialmente los casos límite que pueden ser más propensos a errores.
- Utilizar variables significativas para los mensajes de error en las pruebas, para poder identificar fácilmente dónde ocurrió un error.
- Probar funciones y comandos individuales antes de utilizarlos en pruebas más complejas.

Con estas prácticas en mente, escribir pruebas en Fish Shell puede ser una herramienta valiosa para garantizar un código de mayor calidad.

## Ver también:

- [Documentación de tests en Fish Shell](https://fishshell.com/docs/current/commands.html#test)
- [Ejemplos de pruebas en Fish Shell](https://github.com/fish-shell/fish-shell/tree/master/test)
- [Tutorial sobre pruebas en Fish Shell](https://jvns.ca/blog/2019/08/30/fish--the-test-command/)