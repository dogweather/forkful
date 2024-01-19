---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Leer argumentos de línea de comandos es obtener los valores que se especifican en una línea de comandos cuando se ejecuta un programa. Los programadores los utilizan para personalizar la ejecución de los programas.

## ¿Cómo Hacerlo?

Aquí te muestro cómo capturamos argumentos en el shell de fish.

```Fish Shell
function saludo
  echo Hola $argv[1]
end
```
Ejecutamos la función y obtenemos lo siguiente:

```Fish Shell
> saludo Mundo
Hola Mundo
```
Se pasa "Mundo" como argumento y se imprime "Hola Mundo".

## Profundizando

El concepto de lectura de argumentos se remonta a los primeros días de UNIX. En el pasado, muchos shells no permitían acceso directo a argumentos individuales como `argv[1]`. En su lugar, se usaban desplazamientos de argumentos.

Hay soluciones alternativas como usar `for` para iterar a travéz de todos los argumentos:
```Fish Shell
function saludo_alternativo
  for x in $argv
    echo Hola $x
  end
end
```
En fish, $argv es una lista que contiene todos los argumentos. Puedes operar en ella como en cualquier otra lista en fish.

## Ver También 

Para obtener más detalles y algunas recetas geniales, consulta los siguientes enlaces:

- Documentación oficial | [Parámetros y Variables de Estado](https://fishshell.com/docs/current/commands.html#variables)
- Fish Scripting | [Tutorial](https://fishshell.com/docs/3.1/tutorial.html)
- Sitio de la comunidad de Fish Shell | [Obtén Ayuda](https://fishshell.com/community.html)