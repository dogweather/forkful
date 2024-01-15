---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Fish Shell: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comandos?

Si eres un programador o un amante de la tecnología, probablemente ya estés acostumbrado a trabajar con la línea de comandos. Sin embargo, puede que no estés familiarizado con cómo leer y manipular argumentos de línea de comandos en la Fish Shell. En este artículo te explicaremos por qué deberías aprender a hacerlo y cómo puedes hacerlo.

## Cómo hacerlo

Para leer argumentos de línea de comandos en la Fish Shell, simplemente debes utilizar la variable especial `$argv`. Esta variable contiene una lista de todos los argumentos que se pasaron al ejecutar el script o comando en la línea de comandos. Veamos un ejemplo:

```Fish Shell
# Este script suma dos números
echo "La suma de $argv[1] y $argv[2] es igual a $argv[1] + $argv[2]"
```

Si ejecutamos este script en la línea de comandos de la siguiente manera:

```Fish Shell
./sumar_script 2 3
```

Obtendremos la siguiente salida:

```Fish Shell
La suma de 2 y 3 es igual a 2 + 3
```

Como puedes ver, los valores de los argumentos se pueden acceder mediante `$argv[indice]`, donde el índice comienza en 1 (ya que el nombre del script es `$argv[0]`). También puedes acceder a todos los argumentos en una sola cadena con la variable `$arguments`.

## Un vistazo más profundo

La Fish Shell también tiene la capacidad de leer opciones de línea de comandos, que son argumentos precedidos por un guión (`-`). Estos se pueden acceder mediante la variable `$options`, que contiene un diccionario donde la clave es el nombre de la opción y el valor es su valor. Veamos un ejemplo:

```Fish Shell
# Este script suma dos números
echo "La suma de $argv[1] y $argv[2] es igual a $argv[1] + $argv[2]"
echo "El resultado es:"

# Obtener la opción -v que indica si queremos que se muestre el resultado en modo verboso
if test $options['-v'] = true
    set resultado $argv[1] + $argv[2]
    echo "El resultado de $argv[1] + $argv[2] es igual a $resultado"
else 
    echo $argv[1] + $argv[2]
end
```

Si ejecutamos este script de la siguiente manera:

```Fish Shell
./sumar_script -v 2 3
```

Obtendremos la siguiente salida:

```Fish Shell
La suma de 2 y 3 es igual a 2 + 3
El resultado es:
El resultado de 2 + 3 es igual a 5
```

Como puedes ver, se puede acceder a la opción `-v` a través de `$options['-v']`, que devuelve el valor `true` si está presente en la línea de comandos.

## Ver también

Si quieres seguir aprendiendo sobre la Fish Shell, te recomendamos estos recursos:

- Documentación oficial de la Fish Shell (https://fishshell.com/docs/current/index.html)
- Guía de introducción a la Fish Shell por Anish Athalye (https://medium.com/@anishathalye/learning-fish-shell-4e99cb8de87d)