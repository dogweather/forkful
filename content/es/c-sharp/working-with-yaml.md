---
title:                "Trabajando con yaml"
html_title:           "C#: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué

Algunas veces, trabajando con archivos de configuración o datos estructurados, puede resultar más conveniente utilizar YAML en lugar de JSON o XML. Con su sintaxis intuitiva y legible para humanos, YAML se ha vuelto muy popular en el mundo de la programación.

## Cómo hacerlo

Para empezar a trabajar con YAML en C#, primero debemos asegurarnos de tener instalada la última versión de la librería "YamlDotNet". Luego, podemos utilizar la siguiente estructura de código para leer un archivo YAML y deserializarlo en un objeto:

```C#
using System.IO;
using YamlDotNet.Serialization;

var deserializer = new DeserializerBuilder().Build();
var yamlObject = deserializer.Deserialize<object>(new StreamReader("archivo.yaml"));
```

También podemos crear un archivo YAML desde nuestro código, utilizando un objeto y serializándolo en formato YAML:

```C#
using System.IO;
using YamlDotNet.Serialization;

var serializer = new SerializerBuilder().Build();
var yaml = serializer.Serialize(objeto);
File.WriteAllText("archivo.yaml", yaml);
```

Para trabajar con el objeto obtenido o creado, podemos utilizar técnicas de reflexión para acceder a sus propiedades y valores, o utilizar una clase mapeada a medida con la estructura del archivo YAML.

## Profundizando

Aunque YAML es un formato relativamente sencillo de entender, puede volverse más complejo a medida que trabajamos con él. Podemos encontrarnos con situaciones en las que necesitamos realizar un análisis más profundo del archivo, como por ejemplo, validar su estructura o realizar transformaciones en su contenido.

Para esto, podemos utilizar la librería "YamlDotNet.RepresentationModel", que nos permite trabajar directamente con el árbol de nodos del archivo YAML. Este enfoque es más avanzado y nos brinda un mayor control sobre el archivo, pero requiere un conocimiento más profundo de cómo funciona YAML internamente.

## Ver también

- [Documentación de YamlDotNet](https://dotnetyaml.readthedocs.io/en/latest/)
- [Ejemplos de uso de YamlDotNet](https://github.com/aaubry/YamlDotNet/wiki/SamplesOverview)
- [Especificación oficial de YAML](https://yaml.org/)