---
title:                "Trabajando con yaml"
html_title:           "Go: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué se usa?

Trabajar con YAML es una forma flexible y sencilla de almacenar datos estructurados en formato de texto. Los programadores utilizan YAML porque es un lenguaje fácil de leer y escribir, y por su capacidad para ser utilizado en diferentes entornos y lenguajes de programación.

## Cómo hacerlo:

```Go
// Importar el paquete YAML
import "gopkg.in/yaml.v3"

// Definir una estructura para los datos en YAML
type Persona struct {
  Nombre string `yaml:"nombre"`
  Edad int `yaml:"edad"`
  Hobbies []string `yaml:"hobbies"`
}

// Crear datos en formato YAML
datosYAML := `
- nombre: Juan
  edad: 25
  hobbies:
    - Fútbol
    - Viajar
- nombre: María
  edad: 30
  hobbies:
    - Fotografía
    - Cocina
`

// Decodificar los datos YAML en la estructura definida
var personas []Persona
err := yaml.Unmarshal([]byte(datosYAML), &personas)
if err != nil {
  panic(err)
}

// Acceder a los datos
fmt.Println(personas[0].Nombre) // Juan
fmt.Println(personas[1].Hobbies[0]) // Fotografía

// Codificar datos en YAML a partir de una estructura
datos, err := yaml.Marshal(personas)
if err != nil {
  panic(err)
}

fmt.Println(string(datos)) // Imprime en formato YAML

```

## Profundizando:

El formato YAML fue creado en el año 2001 por Clark Evans y Dan Allen, y desde entonces ha ganado popularidad en el mundo de la programación. Una alternativa a YAML es el formato JSON, pero YAML ofrece una sintaxis más legible para los humanos, lo que lo hace muy útil para configurar aplicaciones y sistemas.

En Go, el paquete "gopkg.in/yaml.v3" es una implementación de la especificación YAML 1.2. Permite decodificar y codificar datos en formato YAML utilizando structs y tags, lo que lo hace muy sencillo y eficiente de trabajar.

## Ver también:

- Documentación de la especificación YAML: https://yaml.org/spec/
- Paquete "gopkg.in/yaml.v3": https://pkg.go.dev/gopkg.in/yaml.v3
- Comparación entre YAML y JSON: https://www.edivaldobrito.com.br/sintaxe-yaml-e-json/