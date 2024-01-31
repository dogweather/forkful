---
title:                "Trabajando con JSON"
date:                  2024-01-19
simple_title:         "Trabajando con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-json.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Trabajar con JSON significa manipular un formato de texto ligero para intercambiar datos. Los programadores lo hacen porque es simple, extensible y se integra a la perfección con muchas APIs y servicios web.

## Cómo hacerlo:
```Go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type Usuario struct {
    ID       int      `json:"id"`
    Nombre   string   `json:"nombre"`
    Tags     []string `json:"tags,omitempty"`
}

func main() {
    usuarioJSON := `{"id":1,"nombre":"Juan","tags":["golang","programación"]}`
    
    var usuario Usuario
    
    err := json.Unmarshal([]byte(usuarioJSON), &usuario)
    if err != nil {
        log.Fatalf("Error al decodificar JSON: %v", err)
    }
    
    fmt.Printf("ID: %d, Nombre: %s, Tags: %v\n", usuario.ID, usuario.Nombre, usuario.Tags)
    
    nuevoUsuario := Usuario{ID: 2, Nombre: "Ana"}
    jsonNuevo, err := json.Marshal(nuevoUsuario)
    if err != nil {
        log.Fatalf("Error al codificar JSON: %v", err)
    }
    
    fmt.Println(string(jsonNuevo))
}
```

Salida de muestra:
```
ID: 1, Nombre: Juan, Tags: [golang programación]
{"id":2,"nombre":"Ana"}
```

## Profundización
El intercambio de datos en formato JSON comenzó a popularizarse a mediados de la década de los 2000. Alternativas incluyen XML y YAML, pero JSON sobresale por su simplicidad de uso en JavaScript y otros lenguajes. En Go, la implementación detallada se encuentra en el paquete `encoding/json`, que provee mecanismos para serializar (`Marshal`) y deserializar (`Unmarshal`) datos con control de errores y personalización a través de tags estructurados.

## Ver También
- Documentación oficial del paquete JSON de Go: [https://pkg.go.dev/encoding/json](https://pkg.go.dev/encoding/json)
- Especificación estándar de JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
