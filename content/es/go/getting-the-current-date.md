---
title:                "Obteniendo la fecha actual"
html_title:           "Go: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué? 
Obtener la fecha actual es una tarea común para los programadores. Esta función les permite obtener información precisa sobre el tiempo de ejecución de su código y también les permite realizar operaciones dependientes de la fecha actual.

## Cómo hacerlo:
Utilizando el lenguaje de programación Go, obtener la fecha actual es muy fácil. Simplemente debes utilizar la función `Now()` del paquete `time` y asignarla a una variable. Aquí tienes un ejemplo:

```Go
import "time"

fechaActual := time.Now()
```

El resultado se almacenará en la variable `fechaActual` y estará en formato de tiempo estándar UTC. También puedes utilizar la función `Now().Local()` para obtener la hora local.

## Profundizando:
En cuanto a alternativas, existen diferentes paquetes y bibliotecas disponibles para obtener la fecha actual en Go. Algunos de los más populares son `time`, `strconv` y `daytime`. Puedes investigar más sobre estas opciones para encontrar la que mejor se adapte a tus necesidades.

En cuanto a la implementación, Go utiliza un reloj interno para obtener la fecha y hora actuales. Este reloj se actualiza a medida que cambia la hora del sistema y se sincroniza con servidores de tiempo externos para evitar desfases.

## Ver también:
- Documentación oficial de Go: [función Now() de time](https://pkg.go.dev/time#Now)
- Tutoriales sobre cómo obtener la fecha actual en Go [en](https://www.sohamkamani.com/blog/2017/11/20/golang-date-time-tutorial/) [inglés](https://zetcode.com/golang/datetime/)