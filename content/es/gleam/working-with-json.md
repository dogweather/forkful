---
title:                "Trabajando con JSON"
date:                  2024-01-19
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Trabajar con JSON significa manipular un formato de texto para representar datos estructurados, común en la web. Programadores lo usan por su facilidad de intercambio entre distintas tecnologías.

## Cómo hacerlo:

```gleam
import gleam/json
import gleam/should

pub fn main() {
  let json_string = """{"nombre": "Juan", "edad": 30}"""
  let resultado = json.decode(json_string)

  case resultado {
    Ok(value) -> 
      should.equal(value, json.Object(from_list([ 
        tuple("nombre", json.String("Juan")), 
        tuple("edad", json.Number(30.0))
      ])))
  
    Error(error) -> 
      io.println(error)
  }
}
```
Salida esperada:
```
Ok(Object(from_list([("nombre", String("Juan")), ("edad", Number(30.0))])))
```

## Profundización

Historia: JSON (JavaScript Object Notation) surgió en los 2000s como alternativa a XML.
Alternativas: XML, YAML y protocol buffers son otras opciones para datos estructurados.
Implementación: En Gleam, `gleam/json` maneja JSON usando decodificación/ codificación tipo-segura.

## Véase también

- Documentación oficial de Gleam `json` módulo: https://hexdocs.pm/gleam_stdlib/gleam/json/
- Guía JSON: https://www.json.org/json-es.html
- Comparación entre JSON y XML: https://www.w3schools.com/js/js_json_xml.asp
