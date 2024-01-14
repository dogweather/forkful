---
title:                "Bash: Generando números aleatorios"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios en Bash?

Generar números aleatorios es una habilidad esencial en la programación de Bash. Al utilizar números aleatorios, podemos crear aplicaciones más dinámicas y divertidas, como juegos y sorteos. Además, también puede ser útil en la realización de pruebas y encriptación de datos sensibles. 

## ¿Cómo hacerlo?

¡Es más fácil de lo que piensas! Bash tiene incorporado un comando para generar números aleatorios: `$RANDOM`. Este comando puede ser utilizado en cualquier parte de un script para generar un número aleatorio en un rango específico. Veamos un ejemplo:

```Bash
#!/bin/bash

# genera un número aleatorio entre 1 y 10
echo "Tu número de la suerte es: $((RANDOM % 10 + 1))"
```

El comando `$RANDOM` devuelve un número aleatorio entre 0 y 32767, por lo que podemos utilizar la operación módulo `%` para obtener un número en un rango específico. En el ejemplo, agregamos 1 al resultado para asegurarnos de que el número generado no sea 0.

También podemos utilizar el comando `shuf` para generar una secuencia de números aleatorios. Este comando es más útil cuando necesitamos múltiples números aleatorios en un solo script. Veamos un ejemplo:

```Bash
#!/bin/bash

# genera una secuencia de 5 números aleatorios en un rango del 1 al 20
shuf -i 1-20 -n 5
```
La opción `-i` especifica el rango de números y la opción `-n` indica la cantidad de números que queremos generar.

## Profundizando en la generación de números aleatorios

Si queremos generar números aleatorios más complejos, podemos utilizar la herramienta `openssl` en Bash. Esta herramienta nos permite generar números pseudoaleatorios basados en algoritmos criptográficos seguros. Veamos un ejemplo:

```Bash
#!/bin/bash

# genera un número aleatorio de 16 bytes
openssl rand -hex 16
```

Podemos utilizar la opción `-hex` para obtener un número en formato hexadecimal, lo que lo hace más seguro para su uso en encriptación.

Otra alternativa es utilizar la librería `urandom` en Bash. Esta es una fuente de números aleatorios de alta calidad que se basa en la actividad del sistema. Veamos un ejemplo:

```Bash
#!/bin/bash

# genera un número aleatorio con la librería urandom
dd if=/dev/urandom count=1 2> /dev/null | od -DAn
```

También podemos utilizar la opción `-b` para obtener el número aleatorio en un formato binario.

## Ver también

- Documentación oficial de Bash sobre [generación de números aleatorios](https://www.gnu.org/software/bash/manual/html_node/Shell-Arithmetic.html#Shell-Arithmetic)
- Tutorial sobre [programación en Bash](https://www.tutorialesprogramacionya.com/bashya/)
- Generación de números aleatorios con la herramienta [shuf](https://shapeshed.com/unix-shuf/) en Unix. 

¡Ahora estás listo para generar números aleatorios en tus scripts de Bash! Recuerda siempre compilar tu código y probarlo varias veces para asegurarte de que los números se generan de forma adecuada. ¡Diviértete experimentando con diferentes métodos de generación de números aleatorios en Bash!