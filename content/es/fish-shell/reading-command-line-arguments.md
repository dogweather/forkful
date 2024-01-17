---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Fish Shell: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La lectura de argumentos de línea de comando es una técnica común utilizada por programadores para poder interactuar con sus programas mediante la línea de comandos. Estos argumentos son información adicional que se puede proporcionar al programa al momento de ejecutarlo, lo que permite modificar su comportamiento o realizar tareas específicas de manera más eficiente.

Esta funcionalidad es muy útil para aquellos usuarios avanzados que prefieren realizar tareas mediante comandos en lugar de una interfaz gráfica. También es útil para automatizar tareas mediante scripts o para permitir la personalización de un programa según las necesidades del usuario.

## Cómo Hacerlo:

Fish Shell, al igual que otros intérpretes de línea de comandos, ofrece varias formas de leer y utilizar argumentos de línea de comando. A continuación, se presentan dos ejemplos de cómo hacerlo:

```
# Ejemplo 1:
# Supongamos que nuestro programa se llama "contador" y queremos que cuente la cantidad de argumentos que le pasamos
Fish Shell contador.fish uno dos cuatro
# La salida será:
3

# Ejemplo 2:
# Supongamos que queremos buscar un archivo en una carpeta determinada
Fish Shell buscar.fish -t txt -d /carpeta -n archivo
# La salida será:
El archivo "archivo.txt" ha sido encontrado.
```

Como se puede ver en los ejemplos, los argumentos de línea de comando se pueden pasar después del nombre del programa, separados por espacios. También se pueden utilizar opciones, como en el segundo ejemplo, para proporcionar información adicional al programa.

## Inmersión Profunda

La lectura de argumentos de línea de comando ha sido una funcionalidad presente en los sistemas operativos desde hace décadas. En los sistemas UNIX, es común utilizar la convención de los parámetros con guiones, como en el segundo ejemplo, donde "-t" significa tipo de archivo y "-d" significa directorio. Sin embargo, cada intérprete de línea de comandos tiene su propia forma de trabajar con estos argumentos, por lo que es importante revisar la documentación específica de cada uno.

También existen alternativas a la lectura de argumentos de línea de comando, como por ejemplo el uso de variables de entorno, que pueden ser útiles en ciertas situaciones. Sin embargo, la lectura de argumentos sigue siendo una técnica ampliamente utilizada y recomendada por su simplicidad y eficiencia.

## Ver también

- Documentación oficial de Fish Shell: https://fishshell.com/docs/current/cmds/fish.html
- Tutorial sobre argumentos de línea de comando en Fish Shell: https://www.techgoat.net/fish-shell-command-line-arguments/
- Comparación entre argumentos de línea de comando y variables de entorno: https://www.baeldung.com/linux/command-line-arguments-vs-environment-variables