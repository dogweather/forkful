---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Lua: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué? 
Convertir una cadena de texto a minúsculas es un proceso común en la programación. Consiste en transformar todas las letras en una cadena de texto a su forma minúscula equivalente. Esto puede ser útil para comparar cadenas de texto de manera más precisa o para estandarizar el formato de una entrada de usuario, entre otros usos. 

## Cómo hacerlo: 
```Lua
-- Ejemplo 1: Usando la función string.lower() 
``` 
cadena = "¡HoLa, MuNdO!"
print(string.lower(cadena)) -- resultado: ¡hola, mundo!

```Lua
-- Ejemplo 2: Utilizando un bucle para convertir cada letra a minúscula 
``` 
cadena = "BIENVENIDO"
nueva_cadena = ""
for i = 1, #cadena do
  nueva_cadena = nueva_cadena .. string.lower(string.sub(cadena, i, i))
end
print(nueva_cadena) -- resultado: bienvenido 

## Profundizando:
La conversión de cadenas a minúsculas no siempre ha sido una operación tan fácil como lo es hoy en día. En los inicios de la programación, las computadoras sólo podían manejar letras en mayúsculas y los programadores tenían que realizar cálculos complejos para convertir letras en minúsculas. Hoy en día, existen alternativas como la función string.upper() que convierte letras a su forma mayúscula.

## Ver también:
Para más información sobre la manipulación de cadenas de texto en Lua, puedes visitar la documentación oficial: https://www.lua.org/manual/5.3/manual.html#6.4.3. También puedes explorar otras funciones útiles para el manejo de cadenas como string.gsub(), string.len(), entre otras.