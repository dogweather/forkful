---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:55.244930-07:00
description: "El registro (logging) en el desarrollo de software es el proceso de\
  \ grabar informaci\xF3n sobre la ejecuci\xF3n de un programa, dise\xF1ado para rastrear\
  \ su\u2026"
lastmod: '2024-03-13T22:44:58.474867-06:00'
model: gpt-4-0125-preview
summary: "El registro (logging) en el desarrollo de software es el proceso de grabar\
  \ informaci\xF3n sobre la ejecuci\xF3n de un programa, dise\xF1ado para rastrear\
  \ su comportamiento y diagnosticar problemas."
title: Registro
weight: 17
---

## Cómo hacerlo:
En Go, el registro puede ser implementado usando el paquete de la biblioteca estándar `log`. Este paquete proporciona capacidades de registro sencillas, como escribir en la salida estándar o en archivos. Comencemos con un ejemplo básico de registro en la salida estándar:

```go
package main

import (
	"log"
)

func main() {
	log.Println("Esta es una entrada de registro básica.")
}
```

Salida:
```
2009/11/10 23:00:00 Esta es una entrada de registro básica.
```

El sello de tiempo al principio de la entrada del registro es añadido automáticamente por el paquete `log`. A continuación, exploramos cómo registrar en un archivo en vez de en la salida estándar:

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("Esta entrada de registro va a un archivo.")
}
```

Ahora, implementemos un caso de uso más avanzado: personalizar el formato del registro. Go te permite crear un registrador personalizado con `log.New()`:

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "REGISTRO PERSONALIZADO: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("Este es un mensaje de registro personalizado.")
}
```

Salida:
```
REGISTRO PERSONALIZADO: 2009/11/10 23:00:00 main.go:11: Este es un mensaje de registro personalizado.
```

Este ejemplo prefija cada mensaje de registro con "REGISTRO PERSONALIZADO: " e incluye la fecha, hora y ubicación del archivo fuente.

## Análisis Detallado
El paquete `log` de la biblioteca estándar de Go es sencillo y suficiente para muchas aplicaciones, pero le faltan algunas de las características más sofisticadas encontradas en bibliotecas de registro de terceros, como registro estructurado, rotación de registros y registro basado en niveles. Paquetes como `zap` y `logrus` ofrecen estas características avanzadas y son bien considerados en la comunidad de Go por su rendimiento y flexibilidad.

El registro estructurado, por ejemplo, te permite registrar datos en un formato estructurado (como JSON), lo cual es especialmente útil para aplicaciones modernas basadas en la nube donde los registros pueden ser analizados por varias herramientas o servicios. `zap`, en particular, es conocido por su alto rendimiento y bajo overhead de asignación, lo que lo hace adecuado para aplicaciones donde la velocidad y eficiencia son críticas.

Históricamente, el registro en Go ha evolucionado significativamente desde la creación del lenguaje. Las primeras versiones de Go proporcionaron las capacidades básicas de registro que vemos en el paquete `log`. Sin embargo, a medida que el lenguaje crecía en popularidad y la complejidad de las aplicaciones escritas en Go aumentaba, la comunidad comenzó a desarrollar bibliotecas de registro más sofisticadas para satisfacer sus necesidades. Hoy en día, mientras que el paquete `log` estándar sigue siendo una opción viable para aplicaciones simples, muchos desarrolladores recurren a estas soluciones de terceros para requisitos de registro más complejos.
