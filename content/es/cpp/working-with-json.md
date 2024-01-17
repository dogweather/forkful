---
title:                "Trabajando con json"
html_title:           "C++: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-json.md"
---

{{< edit_this_page >}}

Hola a todos los programadores, en este artículo hablaremos sobre cómo trabajar con JSON en C++. Esto puede resultar útil ya que JSON es un formato de intercambio de datos muy popular en la web y como programadores siempre necesitaremos manipular datos en algún momento.

## ¿Qué y por qué?
JSON (JavaScript Object Notation) es un formato de intercambio de datos ligero y fácil de leer, basado en el lenguaje de programación JavaScript. Muchas API y servicios web utilizan JSON para enviar y recibir datos, por lo que es importante que los programadores sepan cómo trabajar con este formato. 

## Cómo:
Para trabajar con JSON en C++, hay varias bibliotecas disponibles, como "jsoncpp", "rapidjson" y "nlohmann/json". A continuación, utilizaremos la biblioteca "nlohmann/json" como ejemplo.

Primero, debemos incluir la biblioteca en nuestro código:

```
#include <nlohmann/json.hpp>
```

Luego, podemos crear un objeto JSON y agregar datos a él de la siguiente manera:

```
nlohmann::json data;

data["nombre"] = "Juan";
data["edad"] = 25;
data["hobbies"] = {"programar", "hacer deporte", "viajar"};
```

Podemos imprimir el objeto JSON en la consola utilizando la función "dump":

```
std::cout << data.dump() << std::endl;
```

El resultado será el siguiente:

```
{
    "nombre": "Juan",
    "edad": 25,
    "hobbies": [
        "programar",
        "hacer deporte",
        "viajar"
    ]
}
```

También podemos acceder a los valores específicos del objeto utilizando su clave:

```
std::cout << data["nombre"] << std::endl;
```

El resultado será:

```
Juan
```

## Deep Dive:
JSON fue creado originalmente por Douglas Crockford en 1999, pero no fue hasta 2006 cuando se publicó la primera especificación oficial. Desde entonces, ha ganado popularidad gracias a su fácil lectura y escritura por humanos y máquinas.

Hay varias alternativas a JSON, como XML o YAML, pero JSON sigue siendo el formato más popular debido a su simplicidad y eficiencia.

La biblioteca "nlohmann/json" utiliza una interfaz de usuario basada en la STL y no depende de ninguna otra biblioteca externa, lo que la hace fácil de usar y portable.

## Vea también:
Puedes encontrar más información sobre la biblioteca "nlohmann/json" en su [sitio oficial](https://github.com/nlohmann/json) y una lista de otras bibliotecas para trabajar con JSON en C++ [aquí](https://github.com/miloyip/nativejson-benchmark). ¡Feliz programación!