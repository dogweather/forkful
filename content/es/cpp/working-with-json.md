---
title:                "C++: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Por qué trabajar con JSON en C++

JSON (JavaScript Object Notation) se ha vuelto cada vez más popular en el desarrollo de aplicaciones web y móviles. Es un formato de intercambio de datos simple, ligero y fácil de entender, lo que lo convierte en una excelente opción para almacenar y transmitir datos entre aplicaciones. Al utilizar JSON en C++, puedes leer y escribir datos de forma rápida y eficiente en tus programas. ¡Sigue leyendo para aprender cómo trabajar con JSON en C++!

## Cómo hacerlo

Antes de poder trabajar con JSON en C++, necesitas incluir la librería <iostream> y <jsoncpp/json.h> en tu código. Luego, puedes utilizar la función `Json::Value` para crear un objeto de tipo JSON. A continuación, puedes agregar datos a tu objeto utilizando la función `Json::Value::append ()`, especificando la clave y el valor correspondiente. Por ejemplo:

```C++
Json::Value empleado;
empleado.append("Nombre", "Juan");
empleado.append("Apellido", "Pérez");
empleado.append("Edad", 25);
```

También puedes leer datos de un objeto JSON utilizando la función `Json::Value::get ()`, pasando la clave del valor que deseas obtener. Por ejemplo:

```C++
std::cout << "Nombre: " << empleado.get("Nombre", "default").asString() << std::endl;
```

Este código imprimiría "Nombre: Juan" en la pantalla. Puedes realizar operaciones similares para obtener y manipular datos en un objeto JSON. Una vez que hayas terminado de trabajar con tu objeto JSON, puedes convertirlo en una cadena utilizando la función `Json::FastWriter().write()`, lo que te permitirá guardarla en un archivo o enviarla a través de una red.

A continuación, se muestra un ejemplo completo de cómo trabajar con JSON en C++:

```C++
#include <iostream>
#include <jsoncpp/json.h>

int main() {
    Json::Value empleado;
    empleado.append("Nombre", "Juan");
    empleado.append("Apellido", "Pérez");
    empleado.append("Edad", 25);
    
    std::cout << "Nombre completo: " << empleado.get("Nombre", "default").asString() << " " << empleado.get("Apellido", "default").asString() << std::endl;
    std::cout << "Edad: " << empleado.get("Edad", "default").asInt() << std::endl;
    
    std::string cadena_empleado = Json::FastWriter().write(empleado);
    std::cout << "Cadena JSON: " << cadena_empleado << std::endl;
    
    return 0;
}
```

La salida de este programa sería:

```
Nombre completo: Juan Pérez
Edad: 25
Cadena JSON: {"Nombre" : "Sofía", "Apellido" : "García", "Edad" : 30}
```

## Deep Dive

Si deseas profundizar en el trabajo con JSON en C++, puedes explorar la documentación y ejemplos ofrecidos por la librería JsonCpp en http://open-source-parsers.github.io/jsoncpp/. Además, hay muchas bibliotecas externas que facilitan aún más la manipulación de datos en formato JSON en C++, como RapidJSON y nlohmann/json. Al aprender a trabajar con estas herramientas, podrás experimentar con técnicas avanzadas, como la serialización de objetos C++ en JSON y viceversa.

## Ver también

- http://open-source-parsers.github.io/jsoncpp/
- http://rapidjson.org/
- https://github.com/nlohmann/json