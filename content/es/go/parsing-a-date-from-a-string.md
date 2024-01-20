---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

"Analizar una fecha desde una cadena" implica convertir una fecha escrita en texto a una estructura de fecha en Go. Esta técnica permite procesar fechas en texto de una manera que el programa pueda entender y manipular.

## Cómo hacerlo:

El paquete `time` de Go ofrece funciones para este propósito. Aquí hay un ejemplo:

```Go
paquete principal

importar (
  "fmt"
  "time"
)

func main() {
  layout := "2006-01-02"
  str := "2020-09-30"
  t, err := time.Parse(layout, str)

  if err != nil {
    fmt.Println(err)
  }
  fmt.Println(t)
}
```

Al correr este código, el resultado será:

```Go
2020-09-30 00:00:00 +0000 UTC
```

## Profundización:

(1) En términos históricos, este método de análisis proviene de la necesidad de trabajar con fechas en diversos formatos y zonas horarias en la programación a medida que el mundo se volvía más interconectado.

(2) Alternativamente, hay muchos paquetes de terceros que también pueden parsear fechas, cada uno con sus propias funcionalidades y ventajas.

(3) La función `time.Parse` utiliza un diseño de referencia basado en la fecha `"2006-01-02 15:04:05"`. Cada componente numérico se sustituye por la parte correspondiente de la cadena de fechas que intentamos analizar.

## Ver También:

- El paquete time: [https://golang.org/pkg/time/](https://golang.org/pkg/time/) 
- Go por Ejemplo: Time: [https://gobyexample.com/time](https://gobyexample.com/time)