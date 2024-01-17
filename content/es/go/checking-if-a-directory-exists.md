---
title:                "Comprobando si existe un directorio"
html_title:           "Go: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Comprobar si un directorio existe es una tarea común en la programación. Permite a los programadores verificar si un directorio específico existe en una ubicación determinada y tomar las medidas correspondientes en consecuencia. Esta comprobación es especialmente útil cuando se trabaja con archivos y directorios en lugar de almacenamiento en línea.

## Cómo hacerlo:

```Go
func main() {
	// Creamos un directorio ficticio para demostrar el proceso
	err := os.Mkdir("mi_directorio", 0755)
	if err != nil {
		fmt.Println("Error al crear el directorio:", err)
	} else {
		fmt.Println("Directorio creado correctamente.")
	}

	// Comprobamos si el directorio existe utilizando la función "Exists" de la biblioteca "path/filepath"
	existe, err := filepath.Exists("mi_directorio")
	if err != nil {
		fmt.Println("Error al comprobar si el directorio existe:", err)
	} else if existe {
		fmt.Println("El directorio existe.")
	} else {
		fmt.Println("El directorio no existe.")
	}
}
```

## Profundizando:

Comprobar si un directorio existe es una tarea esencial para trabajar con archivos y directorios en Go. Anteriormente, se utilizaba la función "os.Stat" para este propósito, pero esta función solo funciona en sistemas Unix. Afortunadamente, la biblioteca "path/filepath" proporciona la función "Exists" que funciona en todos los sistemas operativos.

## Véase también:

- Documentación oficial de Go sobre "os.Stat": https://golang.org/pkg/os/#Stat
- Documentación oficial de Go sobre "path/filepath": https://golang.org/pkg/path/filepath/