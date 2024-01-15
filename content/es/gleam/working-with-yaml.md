---
title:                "Trabajando con yaml"
html_title:           "Gleam: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué

YAML es un formato de serialización de datos muy utilizado en el mundo de la programación. Es un lenguaje simple y legible para humanos que permite representar datos estructurados de una manera fácil de entender y procesar. En este artículo, aprenderás cómo trabajar con YAML utilizando el lenguaje de programación Gleam.

## Cómo hacerlo

Para empezar a trabajar con YAML en Gleam, primero debes importar el módulo `yaml` en tu archivo de código:

```Gleam
import yaml
```

Una vez que hayas importado el módulo, puedes utilizar la función `yaml.decode` para convertir un archivo YAML en un objeto de Gleam:

```Gleam
yaml_string = "name: John\nage: 25"
person = yaml.decode(yaml_string)
```

En este ejemplo, estamos utilizando una cadena de texto con datos en formato YAML y la función `yaml.decode` para convertirla en un objeto de Gleam llamado `person`. Ahora podemos acceder a los datos en el objeto de la siguiente manera:

```Gleam
person.name // Retorna "John"
person.age // Retorna 25
```

También podemos utilizar la función `yaml.encode` para convertir un objeto de Gleam en una cadena de texto con formato YAML:

```Gleam
person = {name: "John", age: 25}
yaml_string = yaml.encode(person) // Retorna "name: John\nage: 25"
```

Puedes utilizar estas funciones para trabajar con archivos YAML en tu código de Gleam, ya sea para leer datos de un archivo o para guardar datos en uno nuevo.

## Profundizando

Ahora que sabes cómo utilizar YAML en Gleam, aquí hay algunos consejos adicionales para trabajar de manera más eficiente con este formato de datos.

- Si estás trabajando con un archivo YAML grande, puedes utilizar la función `yaml.decode_stream` en lugar de `yaml.decode`. Esto te permitirá leer el archivo de forma progresiva en lugar de cargarlo completamente en la memoria.
- Si necesitas validar un archivo YAML, puedes utilizar la función `yaml.validate` para asegurarte de que cumpla con la estructura adecuada antes de intentar convertirlo en un objeto de Gleam.
- Puedes utilizar la función `yaml.decode_file` para leer directamente un archivo YAML en lugar de tener que cargarlo primero en una cadena y luego decodificarlo.

Con estas herramientas, trabajar con YAML en Gleam será mucho más sencillo y eficiente.

## Ver también

- [Documentación oficial de YAML](https://yaml.org/)
- [Página de inicio de Gleam](https://gleam.run/)
- [Ejemplos de código en Gleam](https://github.com/gleam-lang/gleam/tree/main/examples)