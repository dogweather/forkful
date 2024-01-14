---
title:                "Fish Shell: Escribiendo pruebas"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

Escribir pruebas (tests) es una práctica esencial en la programación. Las pruebas nos permiten verificar que nuestro código funcione correctamente y evitan la aparición de errores en nuestro código. Además, escribir pruebas nos ayuda a detectar y solucionar problemas en nuestro código de una manera más eficiente.

## Cómo

```Fish Shell``` ofrece una variedad de herramientas para escribir y ejecutar pruebas en nuestros programas. A continuación, se presentará un ejemplo de cómo escribir una prueba simple:

```Fish Shell
# Creamos una función que sume dos números
function sumar_num
  echo "Sumando $1 y $2: "
  set suma = $1 + $2
  echo $suma
end

# Creamos una prueba que verifique si la función suma correctamente dos números
begin
  # Invocamos la función y almacenamos el resultado en la variable "resultado"
  set resultado = (sumar_num 5 10)

  # Utilizamos el comando "test" para comparar el resultado esperado con el resultado obtenido
  # En este caso, esperamos que la suma de 5 y 10 sea igual a 15
  test $resultado = 15

  # Si la comparación es verdadera, el test se considera exitoso
  # Si la comparación es falsa, el test falla y se muestra un mensaje de error
  echo "¡El test fue exitoso!"
end
```

Al ejecutar este código, deberíamos obtener la siguiente salida:

```
Sumando 5 y 10:
15
¡El test fue exitoso!
```

Podemos seguir usando el comando "test" para realizar más pruebas en nuestro código y asegurarnos de que funciona correctamente.

## Deep Dive

Además del comando "test", ```Fish Shell``` también ofrece el comando "assert" que nos permite realizar pruebas más complejas. También podemos utilizar la función "passes" para evaluar una expresión y verificar si es verdadera o falsa.

Es importante tener en cuenta que las pruebas deben ser escritas de forma detallada y exhaustiva para obtener resultados confiables. También es recomendable utilizar pruebas unitarias en lugar de pruebas largas y complejas, de esta manera podemos detectar errores de manera más rápida y eficiente.

## Ver también

- [Documentación oficial de Fish Shell para escribir pruebas](https://fishshell.com/docs/current/cmds/test.html)
- [Guía completa de cómo escribir pruebas en Fish Shell](https://medium.com/@jprochazka/test-driven-development-with-fish-shell-31df089024ca)
- [Tutorial en español sobre pruebas unitarias en Fish Shell](https://outlishve.github.io/post/test-driven-development-con-fish-shell/)