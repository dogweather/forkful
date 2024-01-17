---
title:                "Imprimiendo salida de depuración"
html_title:           "Fish Shell: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Imagina que estás escribiendo un programa y algo sale mal. ¿Cómo lo solucionarías? Aquí es donde entra en juego la impresión de la salida de depuración. Los programadores la usan para encontrar errores y problemas en su código y arreglarlos.

## Cómo:

```Fish Shell``` viene con un comando incorporado llamado ```echo``` que te permite imprimir mensajes en la pantalla. Para añadir un mensaje de depuración en tu código, simplemente coloca el comando ```echo``` seguido del mensaje entre comillas dentro de tu código.

Por ejemplo:

```
function sumar
  echo "Sumando dos números"
  echo "Resultado: "(expr $argv[1] + $argv[2])
end
```

La salida de esto sería:

```
Sumando dos números
Resultado: 6
```

## Deep Dive:

La impresión de la salida de depuración se originó en los primeros días de la programación, cuando los programadores tenían que inspeccionar manualmente cada línea de código para buscar errores. Con el tiempo, se desarrollaron programas y herramientas que facilitaron este proceso y ahora se ha convertido en una práctica común en todas las formas de programación.

Algunas alternativas a la impresión de salida de depuración incluyen el uso de herramientas de depuración específicas, como GDB o Xdebug, o el uso de métodos de depuración más avanzados, como la introspección.

En cuanto a la implementación, ```echo``` es solo uno de los muchos comandos disponibles en ```Fish Shell``` que facilitan la impresión de la salida de depuración. Otros comandos útiles incluyen ```printf``` para imprimir formatos específicos y ```eerror``` para imprimir mensajes de error.

## Ver también:

- [Documentación de Fish Shell](https://fishshell.com/docs/current/)
- [Debugging con GDB](https://www.gnu.org/software/gdb/)
- [Introspección en programación](https://es.wikipedia.org/wiki/Introspecci%C3%B3n_(programaci%C3%B3n)).