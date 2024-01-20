---
title:                "Trabajando con json"
html_title:           "C#: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Trabajar con JSON, o JavaScript Object Notation, es una forma popular de almacenar y transmitir datos en formato de texto. Los programadores lo utilizan porque es fácil de leer y escribir, y porque es compatible con muchos lenguajes de programación diferentes.

## Cómo hacerlo:

Para trabajar con JSON en C#, hay algunas librerías que pueden ser de ayuda. Primero, se debe instalar la librería "Newtonsoft.Json" desde NuGet. Luego, se debe importar a la clase con el siguiente código:

```c#
using Newtonsoft.Json;
```

Ahora, se pueden utilizar métodos como "DeserializeObject" y "SerializeObject" para convertir los datos de JSON a objetos y viceversa. Aquí hay un ejemplo de cómo convertir un objeto de C# en formato JSON:

```c#
string json = JsonConvert.SerializeObject(objeto);
```

En este ejemplo, "objeto" representa el objeto de C# que queremos convertir a JSON. El resultado, "json", será un string con la representación en JSON del objeto.

## Inmersión Profunda:

JSON fue desarrollado en los años 90 como una alternativa al formato de intercambio de información basado en XML. Aunque XML sigue siendo ampliamente utilizado en ciertos contextos, JSON ha ganado popularidad en los últimos años debido a su simplicidad y legibilidad.

Además de Newtonsoft.Json, también existen otras librerías para trabajar con JSON en C#, como System.Text.Json y Utf8Json. Cada una tiene sus propias características y ventajas, por lo que es importante investigar y decidir cuál se ajusta mejor a las necesidades del proyecto.

En cuanto a la implementación, JSON utiliza llaves y valores para almacenar los datos, lo que lo hace similar a un objeto en lenguajes orientados a objetos. También es compatible con arrays, lo que permite almacenar listas de datos de manera eficiente.

## Ver también:

- [Documentación oficial de Newtonsoft.Json](https://www.newtonsoft.com/json)
- [Tutorial de C# y JSON en Code Project](https://www.codeproject.com/Tips/79435/Working-with-JSON-in-C)