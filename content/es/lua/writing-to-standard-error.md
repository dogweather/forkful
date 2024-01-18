---
title:                "Escribir en el error estándar"
html_title:           "Lua: Escribir en el error estándar"
simple_title:         "Escribir en el error estándar"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Es común que los programadores escriban a la salida estándar para imprimir mensajes de error en sus programas. Esto les ayuda a encontrar y solucionar problemas en el código de manera más eficiente. 

## Cómo hacerlo: 
Utilizar la función `io.stderr:write()` para escribir un mensaje a la salida de error estándar en Lua. También se pueden utilizar las funciones `print()` o `io.write()` para escribir a la salida estándar, pero es recomendable utilizar `io.stderr:write()` específicamente para los mensajes de error. 

Ejemplo de código:
```Lua
io.stderr:write("¡Hubo un error en el programa!\n")
print("Este mensaje aparecerá en la salida estándar.")
```

Salida:
```
¡Hubo un error en el programa!
Este mensaje aparecerá en la salida estándar.
```

## Profundizando:
Escribir a la salida de error estándar se ha convertido en una práctica estándar en muchos lenguajes de programación. Esto se debe a que ayuda a los desarrolladores a identificar y corregir errores de manera más rápida. Además, esta práctica es especialmente útil en casos en los que la salida de error no se visualiza directamente, por ejemplo, cuando se ejecuta un programa en segundo plano.

En Lua, además de `io.stderr:write()`, también se puede utilizar la función `error()` para imprimir mensajes de error y lanzar una excepción en el programa. Sin embargo, es importante tener en cuenta que esta función detendrá la ejecución del programa, mientras que `io.stderr:write()` simplemente imprimirá el mensaje de error y permitirá que el programa continúe ejecutándose. 

## Ver también:
- [Documentación oficial de Lua sobre la función `io.stderr:write()`](https://www.lua.org/manual/5.4/manual.html#pdf-io.stderr)
- [Preguntas frecuentes sobre Lua en Stack Overflow](https://es.stackoverflow.com/questions/tagged/lua)