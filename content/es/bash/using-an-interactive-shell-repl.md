---
title:                "Usando una shell interactiva (REPL)"
date:                  2024-01-26T04:11:05.859846-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando una shell interactiva (REPL)"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
REPL significa Bucle de Leer-Evaluar-Imprimir, un entorno de programación de computadoras simple e interactivo. Los programadores lo utilizan para escribir y probar código rápidamente, experimentar con la sintaxis y aprender conceptos de programación sin la sobrecarga de crear y ejecutar aplicaciones completas.

## Cómo hacerlo:
En Bash, tu terminal es esencialmente un REPL. Escribes un comando; lo lee, lo evalúa, imprime el resultado y vuelve al bucle esperando tu siguiente comando. Aquí tienes un ejemplo de cómo usar Bash como un REPL:

```Bash
$ echo "¡Hola, Mundo!"
Hola, Mundo!
$ x=$((6 * 7))
$ echo $x
42
```

Tu entrada sigue al indicador `$ `, con la salida impresa en la siguiente línea. Simple, ¿verdad?

## Profundización
Bash, abreviatura de Bourne Again SHell, es la shell predeterminada en muchos sistemas basados en Unix. Es una mejora de la shell Bourne original, construida a finales de los años 1970. Aunque Bash es una herramienta de scripting poderosa, su modo interactivo te permite ejecutar comandos línea por línea.

Al considerar alternativas, tienes el REPL de Python (simplemente escribe `python` en tu terminal), Node.js (con `node`), y IPython, una shell interactiva de Python mejorada. Casi cada lenguaje tiende a tener su propia implementación de REPL.

Por debajo, los REPL son bucles que analizan tu entrada (comandos o código), la ejecutan y devuelven el resultado a stdout (tu pantalla), a menudo utilizando directamente el intérprete del lenguaje. Esta inmediatez de retroalimentación es excelente para aprender y prototipar.

## Ver También
- [Documentación oficial de GNU Bash](https://gnu.org/software/bash/manual/bash.html)
- [Tutorial interactivo Aprende Shell](https://www.learnshell.org/)
- [Sitio Oficial de IPython](https://ipython.org/)
- [REPL.it](https://replit.com/): Un REPL en línea multilenguaje (¡No solo Bash!)
