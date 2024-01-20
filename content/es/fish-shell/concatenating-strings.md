---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenación de Cadenas en Fish Shell

## ¿Qué & Por Qué?

La concatenación de cadenas es el proceso de unir dos o más cadenas de texto. Los programadores lo hacen para formar frases, comandos o instrucciones más complejas.

## Cómo hacerlo:

En Fish Shell, usamos el comando ```echo``` y simplemente ponemos las cadenas juntas.

```fish
set cadena1 'Hola, '
set cadena2 'Mundo!'
echo $cadena1$cadena2
```
Salida:

```fish
Hola, Mundo!
```
Y ahí lo tienen. Simple y directo.

## Inmersión Profunda

La opción de concatenar cadenas en Fish Shell ha cambiado en su última versión, y ahora es mucho más simple y fácil de usar. Históricamente, tenías que usar comandos más complicados para lograr algo similar. Por supuesto, existen otras formas de concatenar cadenas en diferentes lenguajes de programación, cada uno con su propia sintaxis y reglas.

La concatenación de cadenas en Fish Shell se realiza en tiempo de ejecución. El intérprete de Fish Shell es el encargado de decidir cómo concatenar las cadenas, por eso es importante tener en cuenta que no todo será tan rápido si estás concatenando cadenas muy largas.

## Ver También

Aprenda más sobre la programación en Fish Shell mirando los siguientes enlaces: 

- [Tutorial oficial de Fish Shell](https://fishshell.com/docs/3.1/tutorial.html)
- [Cómo concatenar cadenas en otros lenguajes de programación](https://www.learnpython.org/en/String_Concatenation_and_Formatting)

Y ahí lo tienes, un vistazo rápido a la concatenación de cadenas en Fish Shell. ¡Feliz programación!