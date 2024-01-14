---
title:    "Fish Shell: Extrayendo subcadenas"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

##¿Por qué extraer subcadenas en Fish Shell?

Extraer subcadenas en Fish Shell puede ser una tarea muy útil en la programación. Puede ayudarnos a manipular cadenas de texto más grandes y a obtener solo la información que necesitamos. Esto puede facilitar el procesamiento y manipulación de datos en nuestros scripts. En este artículo, te enseñaremos cómo extraer subcadenas utilizando Fish Shell.

##Cómo hacerlo en Fish Shell

Para extraer una subcadena en Fish Shell, podemos utilizar el comando `string sub`. Este comando toma tres parámetros: la cadena original, la posición inicial y la longitud de la subcadena que queremos extraer. Por ejemplo, si queremos extraer los primeros 5 caracteres de la cadena "Hola mundo", podemos usar el siguiente código:

```Fish Shell
string sub "Hola mundo" 1 5
```

Esto nos dará como resultado la subcadena "Hola ".

También podemos utilizar variables en lugar de cadenas fijas. Por ejemplo, podríamos tener una variable `nombre` con el valor "Juan" y una variable `apellido` con el valor "Pérez". Si queremos extraer la primera letra de cada variable y unirlo en una nueva cadena, podríamos hacerlo de la siguiente manera:

```Fish Shell
set nombre "Juan"
set apellido "Pérez"
string sub $nombre 1 1  # resultado: "J"
string sub $apellido 1 1  # resultado: "P"
string join "" $nombre $apellido  # resultado: "JP"
```

##Profundizando en la extracción de subcadenas

El comando `string sub` también nos permite utilizar números negativos en la posición inicial y longitud. Si utilizamos un número negativo en la posición inicial, contará hacia atrás desde el final de la cadena. Si utilizamos un número negativo en la longitud, nos devolverá la subcadena desde el final de la cadena hasta la posición inicial especificada. Por ejemplo:

```Fish Shell
string sub "Hola mundo" -3 -2  # resultado: "nd"
```

También podemos usar comodines en lugar de posiciones y longitudes específicas. Por ejemplo, si queremos extraer todo el texto después de la primera coma en una cadena, podemos hacerlo así:

```Fish Shell
string match -r "(.*),.*" "Juan, Pérez"  # resultado: "Pérez"
```

Aquí, utilizamos la expresión regular `.*` para que coincida con cualquier carácter, luego especificamos la coma, y finalmente, utilizamos `.*` nuevamente para coincidir con cualquier carácter después de la coma.

##Véase también

- Documentación oficial de Fish Shell sobre el comando `string sub`: https://fishshell.com/docs/current/cmds/string.html#string-sub
- Tutorial de Fish Shell para principiantes: https://www.hostinger.es/tutoriales/tutorial-fish-shell/
- Expresiones regulares en Fish Shell: https://fishshell.com/docs/current/cmds/string.html#string-match