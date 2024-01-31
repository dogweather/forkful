---
title:                "Descargando una página web"
date:                  2024-01-20T17:44:22.261695-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descargando una página web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Descargar una página web es obtener su contenido completo a través de Internet. Los programadores lo hacen para analizar datos, verificar disponibilidad o interactuar con servicios en línea.

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
