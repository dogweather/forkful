---
title:                "Trabajando con json"
html_title:           "Go: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-json.md"
---

{{< edit_this_page >}}

## Por qué

Si estás empezando a programar en Go, probablemente te hayas encontrado con el formato JSON en algún momento. Es muy común en el desarrollo de aplicaciones web y es importante que sepas cómo manejarlo correctamente en tu código. En esta guía, te mostraré por qué trabajar con JSON es importante y cómo puedes hacerlo de manera eficiente en Go.

## Cómo hacerlo

Para trabajar con JSON en Go, primero debes importar el paquete `encoding/json`. Luego, puedes usar la función `json.Marshal()` para codificar una estructura de Go en un formato JSON. Por ejemplo:

```Go
type Persona struct {
    Nombre string
    Edad   int
}

p := Persona{"Juan", 27}

json, _ := json.Marshal(p)
fmt.Println(string(json))
// Output: {"Nombre":"Juan","Edad":27}
```

De forma similar, puedes decodificar un objeto JSON en una estructura de Go utilizando la función `json.Unmarshal()`, como se muestra en el siguiente ejemplo:

```Go
jsonString := `{"Nombre":"Maria","Edad":30}`
var p Persona

json.Unmarshal([]byte(jsonString), &p)
fmt.Println(p.Nombre, p.Edad)
// Output: Maria 30
```

## Profundizando

Además de la función `Marshal()` y `Unmarshal()`, el paquete `encoding/json` también ofrece otras funciones útiles para trabajar con JSON en Go. Por ejemplo, la función `json.NewEncoder()` te permite crear un codificador que escribe directamente en un `io.Writer`, lo que puede ser útil cuando deseas escribir el resultado en un archivo en lugar de imprimirlo en la consola.

También puedes utilizar etiquetas de estructura para especificar cómo quieres que se codifique o decodifique un campo en particular. Por ejemplo, puedes utilizar la etiqueta `json:"-"` para omitir un campo en la codificación JSON, o `json:",omitempty"` para omitir un campo vacío.

Otra característica interesante es la capacidad de utilizar `json.RawMessage` como tipo para un campo. Esto te permite acceder a la cadena JSON sin tener que decodificarla, lo que puede ser útil si solo necesitas leer o escribir un campo específico de un objeto JSON complejo.

## Ver también

- La documentación del paquete `encoding/json` de Go: https://golang.org/pkg/encoding/json/
- Una introducción a JSON en Go: https://blog.golang.org/json and https://golang.org/doc/articles/json_and_go.html
- Otras formas de trabajar con datos estructurados en Go: https://golang.org/pkg/encoding/