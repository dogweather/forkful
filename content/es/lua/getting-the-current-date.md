---
title:                "Obteniendo la fecha actual"
html_title:           "Lua: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

¿Qué es y por qué?: Obtener la fecha actual es una parte importante de la programación en Lua. Permite a los programadores obtener información sobre el tiempo actual, lo que puede ser útil para funciones como programar tareas o registrar eventos.

Cómo hacerlo: En Lua, podemos obtener la fecha actual utilizando la función "os.date()". Por ejemplo, si queremos mostrar la fecha actual en formato día/mes/año, podemos escribir:

```lua
print(os.date("%d/%m/%Y"))
```

Esto imprimirá la fecha actual, por ejemplo: "04/09/2021".

También podemos obtener la fecha en otros formatos, como hora, minutos, segundos, entre otros. Por ejemplo:

```lua
print(os.date("%H:%M:%S"))
```

Esto imprimirá la hora actual, por ejemplo: "13:35:48"

Profundizando: La función "os.date()" en Lua está basada en la función homónima en el lenguaje de programación C. Esta función toma como argumento una cadena de formato y devuelve la fecha y/o hora actual en ese formato específico. Además, también podemos utilizar la función "os.time()" para obtener la fecha y hora actual en segundos desde la "época", que se inició el 1 de enero de 1970.

Alternativamente, también podemos utilizar el módulo "os.date" de la biblioteca "datetime", que nos ofrece una serie de funciones adicionales para trabajar con fechas y horas en Lua.

Vea también: Si desea obtener más información sobre cómo trabajar con fechas en Lua, puede consultar la documentación del lenguaje o visitar los siguientes sitios web:

- https://www.lua.org/manual/5.4/manual.html#6.9
- https://www.w3schools.com/lua/lua_date_time.asp
- http://lua-users.org/wiki/DateTimeLibrary