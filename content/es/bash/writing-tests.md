---
title:                "Bash: Creando pruebas"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en Bash

Escribir pruebas en Bash es una manera efectiva de asegurarse de que nuestro código funcione correctamente. Al realizar pruebas, podemos identificar y corregir errores antes de que afecten a nuestro programa en producción. También nos ayuda a tener un código más claro y fácil de mantener, ya que podemos verificar que nuestros cambios no hayan afectado ninguna otra parte del código.

## Cómo escribir pruebas en Bash
Para escribir pruebas en Bash, podemos utilizar herramientas como "pytest-bash" o "bats-core". A continuación, se presentarán ejemplos de cómo utilizar estas herramientas para escribir pruebas efectivas en Bash.

### Ejemplo con pytest-bash
```Bash
# Este es un ejemplo de una función Bash que queremos probar.
# Supongamos que la función suma dos números enteros y devuelve el resultado.

function sumar {
    resultado=$(($1 + $2))
    echo $resultado
}

# Ahora podemos escribir una prueba utilizando pytest-bash.
# Utilizaremos "assert_output" para verificar que la función devuelve el resultado esperado.

assert_output "sumar 5 3" "8"

# Si ejecutamos esta prueba con pytest-bash, deberíamos obtener el siguiente resultado:

# ========= test session starts =========
# platform linux -- Python 3.x.x, pytest-x.x.x, py-x.x.x, pluggy-x.x.x
# rootdir: <current dir>, inifile: pytest.ini, testpaths: tests
# collected X items

# tests/test_my_function.sh . [100%]

# ========= X passed in X.XX seconds =========

```

### Ejemplo con bats-core
```Bash
# Utilizaremos la misma función del ejemplo anterior.

function sumar {
    resultado=$(($1 + $2))
    echo $resultado
}

# Con bats-core podemos escribir pruebas utilizando el comando "run" y el operador "should".
# Verificaremos que la función sume correctamente números enteros positivos y negativos.

@test "Suma de dos números positivos" {
    run sumar 5 3
    [ "$status" -eq 0 ]
    [ "$output" == "8" ]
}

@test "Suma de un número positivo y uno negativo" {
    run sumar 5 -3
    [ "$status" -eq 0 ]
    [ "$output" == "2" ]
}

# Si ejecutamos estas pruebas con bats-core, obtendremos el siguiente resultado:

# ✓ Suma de dos números positivos
# ✓ Suma de un número positivo y uno negativo
# 2 tests, 0 failures

```

## Profundizando en la escritura de pruebas
Es importante tener en cuenta algunos aspectos al momento de escribir pruebas en Bash. Algunas recomendaciones son:

- Utilizar variables de entorno para definir los valores que se utilizan en las pruebas.
- Utilizar la función "setup" para inicializar las variables para todas las pruebas.
- Utilizar la función "teardown" para liberar los recursos utilizados durante las pruebas.
- Escribir un mensaje de error claro en caso de que alguna prueba falle.

Por último, recuerda que debes mantener tus pruebas actualizadas y agregar nuevas pruebas al realizar cambios en tu código.

## Ver también
- [pytest-bash](https://github.com/pytest-dev/pytest-bash)
- [bats-core](https://github.com/bats-core/bats-core)