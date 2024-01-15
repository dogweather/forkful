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

## Por qué

Si estás buscando una manera sencilla de manejar y almacenar datos estructurados en tu programa, YAML es una excelente opción. Con su sintaxis simple y su compatibilidad con múltiples lenguajes de programación, se ha convertido en una herramienta popular para tareas como la configuración de aplicaciones y la gestión de datos.

## Cómo Hacerlo

Primero, necesitará instalar el paquete "gopkg.in/yaml.v2" en su sistema para poder trabajar con YAML en Go. Luego, importe el paquete en su código utilizando la siguiente sintaxis:

```Go
import "gopkg.in/yaml.v2"
```

Ahora, puede crear una estructura de datos en Go para almacenar su información. Por ejemplo, si deseas almacenar la información de un libro, puede crear una estructura con los siguientes campos:

```Go
type Libro struct {
    Titulo  string `yaml:"titulo"`
    Autor   string `yaml:"autor"`
    Genero  string `yaml:"genero"`
    Páginas int    `yaml:"paginas"`
}
```

Para convertir esta estructura en un archivo YAML, utiliza la función "Marshal" del paquete YAML:

```Go
libro := Libro{Titulo: "El Principito", Autor: "Antoine de Saint-Exupéry", Genero: "Ficción", Páginas: 96}

datos, err := yaml.Marshal(libro)
if err != nil {
    log.Fatalf("error: %v", err)
}
fmt.Println(string(datos))
```

La salida de este código será un archivo YAML como este:

```yaml
titulo: El Principito
autor: Antoine de Saint-Exupéry
genero: Ficción
paginas: 96
```

También puedes convertir un archivo YAML en una estructura de datos en Go utilizando la función "Unmarshal". Por ejemplo, si tienes un archivo llamado "libros.yaml" con la información de varios libros, puedes guardarlos en una lista de estructuras de esta manera:

```Go
datos, err := ioutil.ReadFile("libros.yaml")
if err != nil {
    log.Fatalf("error: %v", err)
}

var lista []Libro
err = yaml.Unmarshal(datos, &lista)
if err != nil {
	log.Fatalf("error: %v", err)
}
```

## Profundizando

Además de la creación y lectura de archivos YAML, el paquete "gopkg.in/yaml.v2" cuenta con varias funciones útiles como "Encoder" y "Decoder" para trabajar con flujos de datos, y métodos para actualizar y eliminar campos de una estructura existente. También es posible utilizar etiquetas personalizadas en las estructuras de datos para tener un mayor control sobre la conversión a YAML.

Para obtener más información sobre cómo trabajar con YAML en Go, puedes consultar la documentación oficial del paquete y la guía de referencia en línea.

## Ver también

- [Documentación oficial del paquete YAML en Go](https://pkg.go.dev/gopkg.in/yaml.v2)
- [Guía de referencia en línea de YAML](https://yaml.org/)