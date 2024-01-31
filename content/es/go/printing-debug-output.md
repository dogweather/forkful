---
title:                "Imprimiendo salida de depuración"
date:                  2024-01-20T17:52:38.707112-07:00
model:                 gpt-4-1106-preview
simple_title:         "Imprimiendo salida de depuración"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/printing-debug-output.md"
---

{{< edit_this_page >}}

# ¿Qué y Por Qué?

**Imprimir salida de depuración** es mostrar valores de variables y estados mientras un programa corre, como si estuviéramos echando un vistazo bajo el capó. Los programadores lo hacen para entender lo que está pasando en su código, encontrar bugs y verificar lógica.

# Cómo Hacerlo:

Para imprimir en Go, importas el paquete `fmt` y usas funciones como `Println` o `Printf`. Aquí tienes un ejemplo simple:

```Go
package main

import "fmt"

func main() {
    nombre := "Mundo"
    fmt.Println("Hola,", nombre)
  
    numeroMagico := 42
    fmt.Printf("El número mágico es: %d\n", numeroMagico)
}
```

Salida esperada:

```
Hola, Mundo
El número mágico es: 42
```

# Inmersión Profunda:

Históricamente, la impresión de salidas de depuración proviene de los días de la consola y los registros de impresión. Antes, los desarrolladores no tenían interfaces sofisticadas, dependían de imprimir mensajes en la consola para entender qué pasaba. 

En Go, además de `fmt`, existe el paquete `log` para ayudar con la depuración y la traza de ejecución. Mientras `fmt` es para la impresión general, `log` agrega más contexto como timestamps o archivos y líneas de código.

Un detalle importante de Go es la eficiencia. Para debug en producción, se recomienda utilizar herramientas más avanzadas como `pprof` o un sistema de logging externo como `logrus` que permiten una depuración más controlada y con menor impacto en el rendimiento.

# Ver También:

- Documentación oficial de `fmt`: https://pkg.go.dev/fmt
- Paquete `log` oficial: https://pkg.go.dev/log
- Artículo sobre logging en Go: https://www.datadoghq.com/blog/logging-in-go/
- Go pprof: https://pkg.go.dev/net/http/pprof
