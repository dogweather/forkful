---
title:                "Bash: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas en Bash?

Una de las tareas más comunes en programación es la manipulación de cadenas de texto. A menudo, necesitamos unir o "concatenar" varias cadenas para formar una sola. En Bash, concatenar cadenas puede ser útil para crear mensajes de salida personalizados, generar archivos con nombres específicos o incluso para construir URL. Aunque puede parecer una tarea sencilla, concatenar cadenas en Bash puede ahorrar tiempo y hacer que nuestro código sea más eficiente.

## Cómo concatenar cadenas en Bash

La forma más básica de concatenar cadenas en Bash es utilizando el operador de asignación "=". Por ejemplo, si queremos unir las cadenas "¡Hola!" y "¿Cómo estás?", podemos escribir lo siguiente en nuestro script:

```Bash
saludo="¡Hola!"  # asignamos "¡Hola!" a la variable "saludo"
pregunta="¿Cómo estás?"  # asignamos "¿Cómo estás?" a la variable "pregunta"
mensaje="$saludo $pregunta"  # unimos las dos variables en la variable "mensaje"
echo $mensaje  # mostramos el contenido de la variable "mensaje"
```

El resultado de este código sería:

```
¡Hola! ¿Cómo estás?
```

También podemos concatenar cadenas utilizando la herramienta de expansión de variables. Por ejemplo, si queremos unir la cadena "¡Hola!" con el contenido de una variable llamada "nombre", podemos hacer lo siguiente:

```Bash
saludo="¡Hola!"
nombre="Juan"
mensaje="$saludo $nombre"
echo $mensaje
```

El resultado de este código sería:

```
¡Hola! Juan
```

Incluso podemos concatenar cadenas utilizando la herramienta de sustitución de comandos, que nos permite ejecutar comandos dentro de una cadena. Por ejemplo, si queremos unir la cadena "La fecha de hoy es " con la salida del comando "date", podemos escribir lo siguiente:

```Bash
fecha="La fecha de hoy es $(date)"
echo $fecha
```

El resultado de este código sería:

```
La fecha de hoy es Mié May 19 09:45:27 CEST 2021
```

## Más información sobre concatenación de cadenas en Bash

Además de las herramientas mencionadas anteriormente, Bash también ofrece otras técnicas para concatenar cadenas, como el uso de la sintaxis "<<<", el comando "echo -n" y la herramienta "printf". Estas técnicas pueden ser útiles en diferentes casos y pueden mejorar la eficiencia de nuestro código. También es importante tener en cuenta que Bash tiene ciertas peculiaridades a la hora de manejar caracteres especiales, como espacios en blanco o comillas, que pueden afectar la concatenación de cadenas.

Si deseas profundizar en el tema de concatenar cadenas en Bash, recomendamos revisar la documentación oficial de Bash y experimentar con diferentes técnicas y ejemplos.

## Ver también

- [Documentación de Bash](https://www.gnu.org/software/bash/)
- [Tutorial de concatenación de cadenas en Bash](https://linuxize.com/post/bash-concatenate-strings/)
- [Ejemplos de concatenación de cadenas en Bash](https://www.tutorialkart.com/bash-shell-scripting/bash-concatenate-strings/)