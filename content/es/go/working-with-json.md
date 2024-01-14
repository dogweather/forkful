---
title:                "Go: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con JSON en Go?
Existen varias razones por las que trabajar con JSON en Go puede ser beneficioso. En primer lugar, JSON es un formato de datos popular y ampliamente utilizado en aplicaciones web y móviles. Además, Go tiene una excelente biblioteca estándar para trabajar con JSON, lo que lo hace muy fácil y eficiente.

## Cómo hacerlo:
```Go
package main

import (
    "encoding/json"
    "fmt"
)

type Person struct {
    Name string `json:"name"`
    Age  int    `json:"age"`
}

func main() {
    p := Person{"Juan", 25}
    
    // Codificar estructura en JSON
    jsonPerson, _ := json.Marshal(p)
    fmt.Println(string(jsonPerson))
    
    // Decodificar JSON en estructura
    var decodedPerson Person
    err := json.Unmarshal([]byte(jsonPerson), &decodedPerson)
    if err != nil {
        fmt.Println(err)
    }
    fmt.Println(decodedPerson)
}

```

Este ejemplo muestra cómo codificar una estructura en JSON utilizando la etiqueta "json" para especificar el nombre de los campos en el JSON resultante. También se muestra cómo decodificar JSON en una estructura utilizando la función Unmarshal.

La salida de este código sería:

```json
{"name":"Juan","age":25}
{name:Juan age:25}
```

## Deep Dive:
Go cuenta con un paquete "encoding/json" que proporciona una API fácil de usar para trabajar con JSON. Este paquete incluye funciones para codificar y decodificar estructuras, así como para leer y escribir archivos JSON. También incluye opciones para trabajar con JSON anidado y datos no estructurados.

Otra característica interesante de Go es que permite etiquetar los campos de una estructura con etiquetas "json", lo que le permite especificar cómo desea que se llamen los campos en el JSON resultante. Esto es útil si está trabajando con datos JSON que son sensibles al rendimiento y desea evitar la serialización y deserialización manual de los datos.

En caso de que necesite trabajar con JSON en tiempo real, Go también cuenta con una función "Encoder" que le permite serializar datos en tiempo real sin tener que crear una estructura primero. Esto es útil en aplicaciones en las que los datos pueden cambiar dinámicamente y necesita una forma eficiente de procesarlos.

## Ver también:
- [Paquete encoding/json en la documentación oficial de Go](https://golang.org/pkg/encoding/json/)
- [Go Cookbook: Manipulación de valores JSON](https://blog.golang.org/json-and-go)
- [Gopher Academy: Trabajando con JSON en Go](https://blog.gopheracademy.com/advent-2015/working-with-json/)