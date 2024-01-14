---
title:                "Go: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué trabajar con YAML en Go

YAML es un formato de datos legible por humanos y fácil de entender. Al utilizarlo en Go, podemos almacenar y transmitir datos de una manera clara y eficiente. Además, es ampliamente utilizado en aplicaciones web y en la configuración de infraestructura.

## Cómo trabajar con YAML en Go

Para empezar a trabajar con YAML en Go, primero debemos importar el paquete "gopkg.in/yaml.v2". Luego, podemos usar la función "Unmarshal" para convertir datos YAML en una estructura de Go. Por ejemplo:

```Go
package main

import (
	"fmt"
	"log"

	"gopkg.in/yaml.v2"
)

// Estructura para guardar datos YAML
type Datos struct {
	Nombre  string
	Email   string
	Edad    int
	SitioWeb string
}

func main() {
	// Datos en formato YAML
	datosYAML := `
	nombre: Juan
	email: juan@example.com
	edad: 25
	sitioweb: example.com
	`

	// Creamos una variable para guardar los datos
	datos := Datos{}

	// Convertimos los datos YAML en una estructura de Go
	err := yaml.Unmarshal([]byte(datosYAML), &datos)
	if err != nil {
		log.Fatalf("error: %v", err)
	}

	// Imprimimos los datos
	fmt.Printf("Nombre: %s\nEmail: %s\nEdad: %d\nSitio web: %s\n", datos.Nombre, datos.Email, datos.Edad, datos.SitioWeb)
}
```

La salida de este código sería:

```
Nombre: Juan
Email: juan@example.com
Edad: 25
Sitio web: example.com
```

También podemos usar la función "Marshal" para convertir una estructura de Go en datos YAML. Por ejemplo:

```Go
package main

import (
	"fmt"
	"log"

	"gopkg.in/yaml.v2"
)

// Estructura para guardar datos YAML
type Fruta struct {
	Nombre string `yaml:"nombre"`
	Color  string `yaml:"color"`
}

func main() {
	// Creamos una estructura de Go
	fruta := Fruta{
		Nombre: "Manzana",
		Color:  "Rojo",
	}

	// Convertimos la estructura en datos YAML
	datos, err := yaml.Marshal(&fruta)
	if err != nil {
		log.Fatalf("error: %v", err)
	}

	// Imprimimos los datos YAML
	fmt.Println(string(datos))
}
```

La salida de este código sería:

```
nombre: Manzana
color: Rojo
```

## Profundizando en YAML en Go

Además de las funciones "Unmarshal" y "Marshal", el paquete "gopkg.in/yaml.v2" también incluye otras funciones útiles como "Decode" y "NewDecoder". Además, es importante tener en cuenta que las estructuras de Go deben tener etiquetas de campo (`yaml:"nombre_campo"`) para poder mapear correctamente los datos YAML en la estructura.

## Ver también

- Documentación oficial del paquete YAML en Go: https://pkg.go.dev/gopkg.in/yaml.v2
- Ejemplos de uso de YAML en Go: https://github.com/go-yaml/yaml/tree/v2/examples