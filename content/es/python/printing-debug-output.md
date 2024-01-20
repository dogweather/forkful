---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

El *printing debug output* es utilizado por programadores para rastrear y resolver problemas en su código. Se hace imprimiendo ciertas variables o mensajes en diferentes partes del programa para entender cómo se está comportando el programa.

## Cómo hacerlo:

En Python, puedes usar la función `print()` para *debugging*. Aquí tienes un ejemplo:

```python
def sumar_numeros(a, b):
    print(f"Sumando {a} y {b}")
    suma = a + b
    print(f"La suma es: {suma}")
    return suma

sumar_numeros(3, 5)
```

Este código te dará la siguiente salida:

```
Sumando 3 y 5
La suma es: 8
```

## Profundización

1. **Contexto histórico**: La técnica de imprimir valores para debugear viene desde los primeros tiempos de la programación. ¡Las primeras herramientas de debugging eran simplemente salir y decirle al programador lo que estaba pasando!

2. **Alternativas**: En Python, a parte de `print()`, tenemos otras herramientas como `logging`, que te permite registrar mensajes de debug de manera más configurable. También puedes usar un depurador real integrado como PDB o pudb.

3. **Detalles de Implementación**: `print()` es muy sencillo de usar, pero puede carecer de algunas capacidades como el poder configurar el nivel de verbose. `logging`, por otro lado, te permite ajustar el nivel de verbose y decidir a dónde enviar los mensajes.

## Ver también

* [Módulo de logging de Python](https://docs.python.org/3/library/logging.html)
* [Depurador de Python PDB](https://docs.python.org/3/library/pdb.html)