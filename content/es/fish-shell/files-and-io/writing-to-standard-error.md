---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:05.247384-07:00
description: "C\xF3mo hacerlo: En Fish Shell, puedes escribir en stderr redirigiendo\
  \ tu salida usando `>&2`. Aqu\xED hay un ejemplo b\xE1sico."
lastmod: '2024-03-13T22:44:59.517700-06:00'
model: gpt-4-0125-preview
summary: En Fish Shell, puedes escribir en stderr redirigiendo tu salida usando `>&2`.
title: "Escribiendo en el error est\xE1ndar"
weight: 25
---

## Cómo hacerlo:
En Fish Shell, puedes escribir en stderr redirigiendo tu salida usando `>&2`. Aquí hay un ejemplo básico:

```fish
echo "Este es un mensaje de error" >&2
```

Este comando simplemente hace eco de un mensaje en stderr en lugar de stdout. Si fueras a escribir un script que genere tanto mensajes regulares como de error, podrías hacer algo como esto:

```fish
echo "Iniciando el proceso"
echo "Ocurrió un error" >&2
echo "Proceso completado"
```

Salida de muestra si ejecutas el script y rediriges stderr a un archivo:

```
Iniciando el proceso
Proceso completado
```

El mensaje de error no aparecería en la salida estándar sino que se encontraría en el archivo al cual redirigiste stderr.

En escenarios que requieren un manejo de errores o registro más sofisticado, Fish no viene con bibliotecas integradas diseñadas explícitamente para esto. Sin embargo, puedes aprovechar herramientas externas o escribir funciones para ayudar. Por ejemplo, crear una función de registro simple podría verse así:

```fish
function log_error
    echo $argv >&2
end

log_error "Este es un mensaje de error avanzado"
```

Esta función `log_error` tomará cualquier cadena que le des y la escribirá en stderr. Usar funciones como esta puede ayudar a mantener tu manejo de errores limpio y consistente a lo largo de tus scripts.
