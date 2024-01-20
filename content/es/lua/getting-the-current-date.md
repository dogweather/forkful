---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Obtener la fecha actual en la programación se refiere a capturar la fecha y hora al momento que se solicita. Los programadores lo hacen para rastrear eventos basados en el tiempo, como la última modificación en una base de datos o registrar cuándo se realizó un acceso específico.


## Cómo hacer esto:

En Lua, podemos utilizar el módulo `os` para obtener la fecha y hora actual. Aquí hay un ejemplo:

```Lua
local fecha_actual = os.date("*t")
print(os.date("%c", os.time(fecha_actual)))
```

Esto imprimirá la fecha y hora en tu formato local. Por ejemplo, podrías obtener: "Tue Jun 22 12:11:04 2022".

## Análisis en profundidad:

Hablando históricamente, Lua proporciona una función `date` en su módulo de OS para trabajar con fechas, lo cual es simple y eficaz. Sin embargo, puede que si se requiere manipulaciones más complejas de fecha y hora, se necesite recurrir a librerías externas como LuaDate o Chronos.

La función `os.date(format, tiempo)` en Lua, convierte un tiempo dado (o el tiempo actual, si no se da tiempo) en una cadena, de acuerdo con el formato dado. El asterisco `'*t'` retorna una tabla con la fecha y la hora que desea convertir.

En términos de implementación, Lua utiliza internamente las funciones de la biblioteca estándar de C para operaciones relacionadas con la fecha y la hora. Esto significa que las fechas y horas en Lua están sujetas a las mismas restricciones y comportamientos que se presentan en C.

## Ver también:

Para obtener más información sobre este tema, consulte las siguientes fuentes:

- Documentación oficial de Lua: [https://www.lua.org/manual/5.4/man.html#6.9](https://www.lua.org/manual/5.4/man.html#6.9)
- GitHub de LuaDate: [https://github.com/Tieske/date](https://github.com/Tieske/date)
- Chronos GitHub: [https://github.com/ashkamath/chronos](https://github.com/ashkamath/chronos)