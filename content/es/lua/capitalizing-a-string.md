---
title:                "Capitalizando una cadena de texto"
html_title:           "Lua: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & por qué?
Convertir a mayúsculas una cadena de texto simplemente significa cambiar todas las letras minúsculas de esta a letras mayúsculas. Los programadores lo hacen para normalizar los datos de entrada del usuario y para evitar problemas en la interpretación del texto.

## Cómo hacerlo:
En Lua, podemos usar la función `upper()`. Esta es una función predefinida, que básicamente convierte toda una cadena de texto a letras mayúsculas. Aquí tienes un ejemplo sencillo:

```Lua
texto = "hola mundo"
texto_mayus = string.upper(texto)
print(texto_mayus) 
```

Este script retornará:

```
HOLA MUNDO
```

## Viaje al centro del asunto
Esta funcionalidad existe desde las primeras versiones de los lenguajes de programación. En Lua, capitalizar una cadena es un proceso directo gracias a la función incorporada `string.upper()`. Sin embargo, hay muchas otras formas de hacerlo si deseas evitar la función estándar, aunque no es recomendable.

En cuanto a los detalles de implementación, la función `string.upper()` de Lua opera en el nivel del carácter, cambiando cada carácter individualmente en lugar de manejar la cadena de texto como un todo. Esto significa que este método es efectivo y eficiente sin importar el tamaño de la cadena de entrada.

## Ver también
Para obtener más información sobre las funciones de cadena en Lua, te recomendamos el siguiente recurso:

- Manual de referencia de Lua: https://www.lua.org/manual/5.4/manual.html#6.4 

Para prácticas y ejercicios adicionales sobre la manipulación de cadenas en Lua, puedes consultar:

- Tuna Lua: https://www.tutorialspoint.com/lua/index.htm