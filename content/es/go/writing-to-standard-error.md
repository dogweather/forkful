---
title:                "Escribiendo en el error estándar"
html_title:           "Go: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar, también conocida como "stderr", puede ser útil para imprimir mensajes de error o depuración mientras se ejecuta un programa en Go. Esto permite al desarrollador obtener información sobre posibles errores sin detener la ejecución del programa.

## Cómo hacerlo

Para escribir a la salida de error estándar en Go, se utiliza la función `Fprintln` del paquete `fmt` junto con `os.Stderr` como el primer parámetro. Aquí hay un ejemplo de código que imprimirá un mensaje de error en stderr:

```Go
fmt.Fprintln(os.Stderr, "¡Este es un mensaje de error!")
```

El resultado de este código sería la siguiente salida en la terminal:

```
¡Este es un mensaje de error!
```

## Profundizando

La salida de error estándar es un flujo de datos que se utiliza para imprimir mensajes de error durante la ejecución de un programa. En Go, también se puede usar `os.Stderr` para leer la entrada de error estándar.

Además, la función `Fprint` del paquete `fmt` también se puede utilizar para escribir en la salida de error estándar sin agregar un salto de línea al final del mensaje.

## Ver también

- Documentación oficial de Go sobre el paquete `fmt`: https://golang.org/pkg/fmt/
- Documentación oficial de Go sobre el paquete `os`: https://golang.org/pkg/os/
- Ejemplo práctico de uso de la salida de error estándar en Go: https://gobyexample.com/stderr