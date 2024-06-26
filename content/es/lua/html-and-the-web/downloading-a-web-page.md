---
date: 2024-01-20 17:44:22.261695-07:00
description: "C\xF3mo hacerlo: Usaremos la biblioteca `socket.http` para descargar\
  \ el contenido de una p\xE1gina web. Primero, instala la biblioteca si a\xFAn no\
  \ lo has hecho."
lastmod: '2024-03-13T22:44:59.200573-06:00'
model: gpt-4-1106-preview
summary: "Usaremos la biblioteca `socket.http` para descargar el contenido de una\
  \ p\xE1gina web."
title: "Descargando una p\xE1gina web"
weight: 42
---

## Cómo hacerlo:
Usaremos la biblioteca `socket.http` para descargar el contenido de una página web. Primero, instala la biblioteca si aún no lo has hecho:

```lua
-- Open your terminal and type:
luarocks install luasocket
```

Luego, pega el siguiente código en tu editor y ejecútalo:

```lua
local http = require("socket.http")
local body, code = http.request("http://www.example.com")

if code == 200 then
    print("Contenido descargado:")
    print(body)
else
    print("Error al descargar la página: "..tostring(code))
end
```

Salida de muestra:

```
Contenido descargado:
<!doctype html>
<html>
...
</html>
```

## Profundizando
El proceso de descargar páginas web con Lua no es nuevo, pero ha evolucionado. Antes se usaban herramientas de línea de comandos como `wget` o `curl` a través de `os.execute`. Ahora, `luasocket` ofrece una forma más limpia y directa de hacerlo desde dentro del propio lenguaje.

Como alternativa, puedes explorar `LuaSec` para manejar conexiones HTTPS. Es importante porque muchas páginas requieren una conexión segura. Para instalación:

```lua
luarocks install luasec
```

En términos de implementación, puedes agregar cabeceras HTTP, manejar redirecciones o cookies si es necesario para tu caso de uso.

## Ver también
- Documentación de LuaSocket: http://w3.impa.br/~diego/software/luasocket/http.html
- GitHub de LuaSec: https://github.com/brunoos/luasec
- LuaRocks, el gestor de paquetes de Lua: https://luarocks.org/
