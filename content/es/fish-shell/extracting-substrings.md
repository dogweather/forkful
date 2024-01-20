---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extracción de subcadenas en Fish Shell

## ¿Qué y por qué?
La extracción de subcadenas es el proceso para tomar una porción más pequeña de una cadena de texto más grande. Los programadores realizan este proceso para procesar datos específicos de una fuente más grande.

## ¿Cómo se hace?
Vamos a seguir un camino sencillo y directo.

Extraer subcadenas en Fish Shell es bastante fácil usando el comando `string sub`. Vamos a ver un ejemplo:

```Fish Shell
set -l cadena "Hola, Mundo"
set -l subcadena (string sub -l 5 -- $cadena)
echo $subcadena
```

Este comando extraería las primeras 5 letras de la palabra "Hola, Mundo" y daría como resultado "Hola,".

## Inmersión profunda

Dado que Fish Shell es un shell de scripting relativamente nuevo (2005), es menos probable que encuentres su funcionalidad de extracción de subcadenas en shells más antiguas como bash o sh. Sin embargo, esto no significa que Fish Sea menos poderoso.

Otras alternativas podrían ser el uso de otras herramientas de línea de comandos disponibles en Unix, como cut, awk, o incluso Perl. Cada una de estas alternativas tiene su propio conjunto de ventajas y desventajas en cuanto a rendimiento y legibilidad.

La implementación del comando `string sub` en Fish Shell se basa en la funcionalidad de las bibliotecas de C para el corte y manipulación de cadenas. Esto le permite ser rápido y eficiente, ideal para operaciones de scripts más grandes y más complicadas.

## Ver también
Consulta los siguientes recursos para obtener más información:
1. Tutorial de Fish Shell: [Fish Shell para principiantes](http://fishshell.com/docs/current/tutorial.html)
2. Documentación oficial de Fish Shell: [Comando String](http://fishshell.com/docs/current/cmds/string.html)
3. RECURSO ÚTIL no oficial en Fish Shell: [Syntax guide](https://learnxinyminutes.com/docs/fish/)

Disfrute de su viaje con Fish Shell y la extracción de subcadenas!