---
title:    "Bash: Convirtiendo una cadena a minúsculas"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo se necesita manipular cadenas de texto para realizar diversas tareas. Una de estas tareas puede ser convertir una cadena de texto a minúsculas. Esto es especialmente útil para comparar cadenas de texto de manera más sencilla o para asegurarse de que los datos ingresados por los usuarios sean consistentes y legibles.

## Cómo hacerlo

La conversión de una cadena de texto a minúsculas en Bash es muy sencilla. Simplemente usamos el comando `tr` seguido de la opción `-s` para especificar que queremos aplicar la conversión de forma "silenciosa" y luego usamos la opción `-d` junto con la expresión regular `[A-Z]` para eliminar todas las mayúsculas. Aquí hay un ejemplo:

```Bash
%%bash

cadena="BLOG DE PROGRAMACIÓN"
echo "$cadena" | tr -s -d '[A-Z]'
```

El resultado de este código sería `blog de programación`, todas las letras mayúsculas han sido eliminadas. También podemos usar la opción `-s` junto con `-d` para eliminar los espacios en blanco y hacer que la cadena de texto resultante sea más legible.

```Bash
%%bash

cadena="HOLA, BIENVENIDOS!"
echo "$cadena" | tr -s -d '[A-Z]'
```

Con esto, la salida sería `holabienvenidos`, lo cual puede ser útil para formatear nombres o títulos de manera más concisa.

## Profundicemos

Aunque el comando `tr` es muy útil para convertir una cadena de texto a minúsculas, debemos tener en cuenta que solo funciona para caracteres ASCII. Si necesitamos trabajar con caracteres internacionales, podemos utilizar el comando ` awk` y su función `tolower`:

```Bash
%%bash

cadena="HOLA, BIENVENIDOS!"
awk '{print tolower($0)}' <<< "$cadena"
```

La salida de este código sería `hola, bienvenidos!`, manteniendo los caracteres internacionales intactos.

También es importante mencionar que la conversión a minúsculas puede ser problemática si nuestros datos contienen números o caracteres especiales. En ese caso, es importante asegurarse de que la conversión se aplique solo a las letras y no a los demás caracteres. Esto se puede lograr utilizando una expresión regular específica y la opción `-c` del comando `tr`.

En resumen, convertir una cadena de texto a minúsculas puede ser de gran ayuda en la programación para comparar datos y asegurarse de su consistencia y legibilidad. Sin embargo, debemos tener en cuenta los posibles problemas en cuanto a caracteres internacionales y otros caracteres especiales.

## Ver también

- Bash `tr` command documentation: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- AWK `tolower` function documentation: https://www.gnu.org/software/gawk/manual/gawk.html#Built_002din-Functions