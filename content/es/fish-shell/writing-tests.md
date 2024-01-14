---
title:    "Fish Shell: Redacción de pruebas"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

Escribir pruebas es una práctica esencial para cualquier programador. Al escribir pruebas, podemos verificar el funcionamiento de nuestro código y asegurarnos de que no haya errores antes de implementarlo en producción. Esto nos ahorra tiempo y esfuerzo en el futuro, ya que nos permite detectar y corregir problemas de manera temprana.

## Cómo hacerlo

Para escribir pruebas en Fish Shell, podemos utilizar la herramienta `bass`. Esta herramienta nos permite ejecutar comandos de Bash dentro de nuestro script de Fish Shell, lo que nos permite aprovechar las utilidades de Bash para escribir nuestras pruebas.

Para empezar, debemos instalar `bass` usando el siguiente comando:

```Fish Shell
brew install bass
```

Una vez instalado, podemos escribir nuestras pruebas en forma de funciones de Bash utilizando la sintaxis `bass: <<^…` seguida de nuestro código de prueba y `^…` para cerrar la función. Por ejemplo:

```Fish Shell
function test_suma
  bass: <<^ - 
    
    # Definir variables
    a=10
    b=5

    # Ejecutar suma
    let "resultado = $a + $b"

    # Salida esperada
    if [ $resultado -eq 15 ] 
    then 
    echo "Test exitoso"
    else 
    echo "Test fallido"
   fi
  ^
end
```

El código anterior define una función de prueba llamada `test_suma` que verifica si la suma de dos números es igual a 15 como se espera. Para ejecutar esta prueba, podemos simplemente llamar a la función `test_suma` en nuestro script de Fish Shell.

## Deep Dive

Al escribir pruebas, es importante tener en cuenta algunos aspectos importantes:

- Asegurarnos de que nuestras pruebas sean independientes y no dependan de otras pruebas o variables fuera de la función de prueba.
- Probar diferentes casos de borde para garantizar que nuestro código funcione correctamente en todas las situaciones.
- Nombrar adecuadamente nuestras funciones de prueba para que sean fáciles de entender y ejecutar.

No olvidemos que escribir pruebas no solo es para verificar el funcionamiento de nuestro código, sino también para facilitar el proceso de depuración y detección de errores en el futuro.

## Ver Además

- [Documentación oficial de `bass`](https://github.com/edc/bass)
- [Tutorial de Fish Shell para principiantes](https://fishshell.com/docs/current/tutorial.html)
- [Escribir pruebas en Bash](https://www.baeldung.com/linux/testing-bash-scripts)