---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
La concatenación de cadenas en Lua es el proceso de juntar dos o más cadenas para formar una nueva. Los programadores lo hacen para manipular datos de texto y crear salidas personalizadas en sus programas.

## Cómo se hace:
Puedes concatenar cadenas en Lua usando el operador `..`. Aquí tienes un ejemplo:

```Lua
cadena1 = "Hola, "
cadena2 = "Mundo!"
cadena_concatenada = cadena1 .. cadena2
print(cadena_concatenada)
```

Esto resultará en la salida:

```
Hola, Mundo!
```

## Un Vistazo Más Profundo
Lua, a diferencia de muchos otros lenguajes de programación, usa el operador `..` para la concatenación en lugar del más usual `+`. Esto surgió de las primeras versiones de Lua, donde se hizo una distinción fuerte entre operaciones numéricas y de cadena.

En lugar de operador `..`, también puedes usar la función `string.concat()` en Lua, aunque es menos común.

```Lua
cadena1 = "Hola, "
cadena2 = "Mundo!"
cadena_concatenada = string.concat(cadena1, cadena2)
print(cadena_concatenada)
```

Dará el mismo resultado:

```
Hola, Mundo!
```

La concatenación de cadenas, aunque es simple, puede ser costosa en términos de rendimiento si se realiza en grandes volúmenes. Esto se debe a que cada operación crea una nueva cadena. Para estos casos, Lua ofrece el módulo de biblioteca `table.concat()`, que es más eficiente.

## Ver también
Para más información sobre la manipulación de cadenas en Lua, consulta las siguientes fuentes:
1. [Lua-Users wiki: Strings Tutorial](http://lua-users.org/wiki/StringsTutorial)
2. [Programming in Lua: Strings](https://www.lua.org/pil/2.4.html)
3. [Learn X in Y Minutes: Lua](https://learnxinyminutes.com/docs/es-es/lua-es/)