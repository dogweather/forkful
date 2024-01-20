---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Empezar un nuevo proyecto es como abrir un libro en blanco donde escribes tu propio código. Los programadores hacen esto para experimentar con ideas nuevas, resolver problemas complejos o simplemente aprender algo nuevo.

## ¿Cómo hacerlo?

### Primeros pasos
```
Lua
-- Un simple saludo en Lua
print("¡Hola, Mundo!")
```
Output:
```
¡Hola, Mundo!
```
### Proyecto básico
```
Lua
function saludo(nombre)
    return "¡Hola, " .. nombre .. "!"
end

print(saludo("Programador"))
```
Output:
```
¡Hola, Programador!
```

## Deep Dive

En 1993, Lua fue creada por Roberto Ierusalimschy, Luiz Henrique de Figueiredo, y Waldemar Celes, miembros del equipo Tecgraf en el Instituto Pontifício Universidade Católica do Rio de Janeiro, Brasil. Se diseñó Lua para ser ligera, embebida en aplicaciones, y expansible.

Alternativas a Lua incluyen JavaScript para desarrollo web y Python para propósitos generales de programación. Sin embargo, Lua se distingue por su ligereza y facilidad de embeber en aplicaciones.

Para empezar un nuevo proyecto en Lua, necesitas un editor de texto para escribir tu código y un intérprete de Lua para ejecutarlo. Lua tiene una sintaxis simple que es fácil de leer y escribir.

## See Also

Para más detalles sobre la programación en Lua, echa un vistazo a estos enlaces:

- El Manual de Referencia de Lua: https://www.lua.org/manual/5.4/
- Programación en Lua, primera edición: https://www.lua.org/pil/1.html
- LuaUsers wiki, un recurso para la comunidad de usuarios de Lua: http://lua-users.org/wiki/