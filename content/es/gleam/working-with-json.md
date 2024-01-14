---
title:                "Gleam: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/working-with-json.md"
---

{{< edit_this_page >}}

#Por qué trabajar con JSON en Gleam

Si estás buscando una forma fácil y eficiente de manejar datos en tus proyectos de programación, entonces JSON es la respuesta. JSON (JavaScript Object Notation) es un formato de intercambio de datos ligero y fácil de entender, lo que lo hace ideal para almacenar y transmitir datos en la web. Con Gleam, trabajar con JSON se vuelve aún más sencillo gracias a sus funciones y sintaxis intuitivas.

##Cómo trabajar con JSON en Gleam

Para empezar a trabajar con JSON en Gleam, simplemente debes seguir estos pasos:

1. Importa el módulo JSON en tu archivo Gleam.
2. Crea una estructura de datos con la información que deseas convertir a JSON.
3. Utiliza la función `to_string` para convertirla en una cadena JSON.
4. Si deseas convertir una cadena JSON en una estructura de datos, utiliza la función `from_string`.

Aquí hay un ejemplo de cómo convertir una estructura de datos a JSON en Gleam:

```Gleam
import json

pub struct Person(name: String, age: Int, occupation: String)

let person = Person(name: "Juan", age: 25, occupation: "Programador")

let json_string = json.to_string(person)

// Este es el resultado: {"name": "Juan", "age": 25, "occupation": "Programador"}
```

También puedes utilizar la función `to_json` para imprimir la estructura de datos directamente como JSON en la consola:

```Gleam
import json

pub struct Person(name: String, age: Int, occupation: String)

let person = Person(name: "Juan", age: 25, occupation: "Programador")

io.format("{}", json.to_json(person))

// Output: {"name": "Juan", "age": 25, "occupation": "Programador"}
```

##Profundizando en el trabajo con JSON en Gleam

Además de las funciones mencionadas anteriormente, Gleam también cuenta con otras herramientas para trabajar con JSON. Por ejemplo, puedes utilizar el módulo JSON para analizar datos JSON de una URL o archivo, así como para convertir una estructura de datos a un mapa de JSON. También puedes utilizar la librería `gleam/decode` para decodificar JSON en estructuras de datos definidas por ti.

Es importante tener en cuenta que Gleam trata a JSON como datos inmutables, lo que significa que no se pueden realizar cambios directamente en una cadena JSON. En cambio, puedes utilizar las funciones de Gleam para crear una nueva estructura de datos basada en la información JSON que deseas modificar.

#Vea también

- Documentación oficial de JSON en Gleam: https://gleam.run/modules/json.html
- Ejemplos de uso de JSON en Gleam: https://github.com/gleam-lang/gleam/blob/master/example/json.gleam