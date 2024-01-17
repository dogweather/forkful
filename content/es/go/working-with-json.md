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

## ¿Qué y por qué?
JSON(JavaScript Object Notation) es una forma de almacenar y transmitir datos de forma legible para los humanos y fácil de procesar para las máquinas. Los programadores utilizan JSON para intercambiar información entre diferentes sistemas y aplicaciones de forma rápida y eficiente.

## ¿Cómo hacerlo?
Para trabajar con JSON en Go, es necesario importar el paquete `encoding/json`. Luego, podemos utilizar la función `Marshal` para convertir una estructura de datos en formato JSON y la función `Unmarshal` para convertir una cadena de texto JSON en una estructura de datos en Go.

Ejemplo de código:
```Go
// Datos de ejemplo en una estructura Go
type Persona struct {
	Nombre string `json:"nombre"`
	Edad   int    `json:"edad"`
	Ciudad string `json:"ciudad"`
}

// Convertir la estructura en formato JSON
jsonPersona, err := json.Marshal(Persona)
if err != nil {
    fmt.Println(err)
}
fmt.Println(string(jsonPersona))

// Convertir una cadena de texto JSON en la estructura de datos en Go
textoJSON := `{"nombre": "María", "edad": 25, "ciudad": "Madrid"}`
var persona Persona
err = json.Unmarshal([]byte(textoJSON), &persona)
if err != nil {
    fmt.Println(err)
}
fmt.Println(persona)
```

Salida:
`{"nombre":"María","edad":25,"ciudad":"Madrid"}`
`{Nombre:María Edad:25 Ciudad:Madrid}`

## Profundizando
JSON fue desarrollado en 2001 por Douglas Crockford y se ha vuelto uno de los formatos de intercambio de datos más populares en la actualidad, especialmente en la web. Alternativas a JSON incluyen XML y YAML, pero JSON es preferido debido a su simplicidad y facilidad de uso.

Detalles de implementación:
- En Go, los campos de una estructura de datos deben estar escritos en mayúscula para que sean exportados y puedan ser utilizados en el formato JSON.
- Cuando se utiliza la función `Unmarshal`, es importante pasar un puntero a la estructura de datos para que los cambios se reflejen en la variable original.

## Ver también
- Documentación oficial de Go para el paquete `encoding/json`: https://golang.org/pkg/encoding/json/
- Ejemplos de código en Go para trabajar con JSON: https://gobyexample.com/json