---
title:                "Trabajando con YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qué es y por qué?

Trabajar con YAML significa manipular un formato de serialización de datos legible por humanos, común para configuraciones y transmisión de datos. Los programadores lo usan por su claridad y fácil compatibilidad entre diferentes lenguajes y aplicaciones.

## Cómo hacerlo:

En Go, usamos el paquete `gopkg.in/yaml.v3` para trabajar con YAML. Aquí hay un ejemplo sencillo de cómo leer y escribir datos YAML.

```Go
package main

import (
	"fmt"
	"gopkg.in/yaml.v3"
	"log"
	"os"
)

// Config representa una configuración en formato YAML.
type Config struct {
	Host    string `yaml:"host"`
	Port    int    `yaml:"port"`
	Enabled bool   `yaml:"enabled"`
}

func main() {
	// Datos YAML de ejemplo.
	data := `
host: localhost
port: 8080
enabled: true
`
	var config Config

	// Deserializar los datos YAML a la estructura Config.
	if err := yaml.Unmarshal([]byte(data), &config); err != nil {
		log.Fatalf("error: %v", err)
	}
	fmt.Printf("--- config:\n%v\n\n", config)

	// Modificar la configuración.
	config.Port = 9090

	// Serializar la estructura Config a YAML.
	out, err := yaml.Marshal(&config)
	if err != nil {
		log.Fatalf("error: %v", err)
	}
	fmt.Printf("--- yaml:\n%s\n", string(out))
  
  // Escribir el YAML serializado en un archivo.
	if err := os.WriteFile("config.yaml", out, 0644); err != nil {
		log.Fatalf("error: %v", err)
	}
	fmt.Println("Archivo 'config.yaml' creado exitosamente.")
}
```

Salida de muestra:

```
--- config:
{localhost 8080 true}

--- yaml:
host: localhost
port: 9090
enabled: true

Archivo 'config.yaml' creado exitosamente.
```

## Inmersión Profunda:

YAML, que significa "YAML Ain't Markup Language", surgió a principios de los años 2000 como alternativa al XML y al JSON por ser más legible. Aunque JSON es ampliamente utilizado por su compatibilidad con JavaScript, YAML se destaca en configuraciones debido a su fácil legibilidad y soporte de comentarios. Dentro de Go, el proceso de serialización se denomina 'marshal', y el de deserialización 'unmarshal'. Se puede manipular fácilmente estructuras complejas, trabajar con tipos personalizados y manejar diferentes esquemas de datos.

## Ver También:

- Documentación del paquete YAML para Go: [gopkg.in/yaml.v3](https://pkg.go.dev/gopkg.in/yaml.v3)
- Especificación de YAML: [yaml.org/spec](https://yaml.org/spec/1.2/spec.html)
