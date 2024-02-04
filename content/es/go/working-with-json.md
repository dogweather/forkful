---
title:                "Trabajando con JSON"
date:                  2024-02-03T18:12:07.073139-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Trabajar con JSON (JavaScript Object Notation) en Go implica codificar y decodificar datos entre estructuras de datos de Go y el formato JSON. Esta tarea es ubicua en servicios web y APIs, ya que JSON funciona como un formato de intercambio de datos ligero, basado en texto y independiente del lenguaje, lo que permite compartir datos de manera simple a través de diferentes entornos de programación.

## Cómo hacerlo:

En Go, el paquete `encoding/json` es tu puerta de entrada a la manipulación de JSON, proporcionando mecanismos para convertir estructuras de datos de Go a JSON (marshalling) y viceversa (unmarshalling). A continuación se presentan ejemplos básicos para comenzar:

### Codificación (Marshalling)

Para convertir una estructura de Go a JSON, puedes usar `json.Marshal`. Considere la siguiente estructura de Go:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

Salida:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### Decodificación (Unmarshalling)

Para analizar JSON en una estructura de datos de Go, usa `json.Unmarshal`:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

Dada la estructura `User` como antes, este código analiza la cadena JSON en una instancia de Usuario.

Salida:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## Profundización

El paquete `encoding/json` en Go ofrece una API sencilla que abstrae gran parte de la complejidad involucrada en la manipulación de JSON. Introducido al inicio del desarrollo de Go, este paquete refleja la filosofía de Go de simplicidad y eficiencia. Sin embargo, el uso de la reflexión por `encoding/json` para inspeccionar y modificar estructuras en tiempo de ejecución puede llevar a un rendimiento menos óptimo en escenarios intensivos en CPU.

Han surgido alternativas como `json-iterator/go` y `ffjson`, proporcionando un procesamiento de JSON más rápido mediante la generación de código de marshalling y unmarshalling estático. No obstante, `encoding/json` sigue siendo el paquete más utilizado debido a su simplicidad, robustez y el hecho de que es parte de la biblioteca estándar, asegurando compatibilidad y estabilidad a través de las versiones de Go.

A pesar de su rendimiento relativamente más lento, la facilidad de uso y la integración con el sistema de tipos de Go hacen que `encoding/json` sea adecuado para la mayoría de las aplicaciones. Para aquellos que trabajan en contextos donde el rendimiento es primordial, explorar bibliotecas externas puede ser valioso, pero para muchos, la biblioteca estándar encuentra el equilibrio correcto entre velocidad, simplicidad y fiabilidad.
