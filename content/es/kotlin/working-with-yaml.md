---
title:                "Kotlin: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué trabajar con YAML

YAML (acrónimo de "YAML Ain't Markup Language") es un lenguaje de marcado ligero utilizado para representar datos de manera legible para humanos. Es ampliamente utilizado en aplicaciones de programación para almacenar y transmitir información estructurada. A continuación, te explicamos cómo utilizar YAML en tus proyectos de Kotlin.

## Cómo utilizar YAML en Kotlin

En primer lugar, es necesario importar la biblioteca SnakeYAML en tu proyecto de Kotlin. Esto se puede hacer agregando la siguiente dependencia en tu archivo build.gradle:

```
dependencies {
    compile "org.yaml:snakeyaml:1.27"
}
```

Una vez que hayas importado la biblioteca, puedes comenzar a trabajar con YAML. El primer paso es crear un objeto de tipo `Yaml`, que es el punto de entrada para trabajar con YAML en Kotlin:

```
val yaml = Yaml()
```

A continuación, puedes utilizar la función `load` del objeto `Yaml` para cargar datos YAML desde un archivo o una cadena:

```
val data = yaml.load("""
    - name: John
      age: 30
    - name: Sarah
      age: 25
""")
```

Este código creará una lista de objetos `Map` que contienen la información de cada persona. Puedes acceder a los datos de la siguiente manera:

```
val name = data[0]["name"]  // John
val age = data[1]["age"]  // 25
```

También puedes utilizar la función `dump` para convertir un objeto Kotlin en una cadena YAML:

```
val person = mapOf("name" to "Mark", "age" to 40)
val yamlString = yaml.dump(person)
```

Este código generará la siguiente cadena YAML:

```
name: Mark
age: 40
```

## Profundizando en YAML

Además de cargar y guardar datos YAML, también es posible trabajar con estructuras de datos más complejas, como listas y objetos anidados. Para ello, puedes utilizar la biblioteca `snakeyaml` para mapear estos datos a objetos Kotlin y viceversa.

También es posible personalizar la forma en que se cargan y se guardan los datos YAML mediante el uso de opciones de configuración y anotaciones. Puedes explorar más sobre estas opciones en la documentación de `snakeyaml`.

¡Ahora estás listo para comenzar a trabajar con YAML en tus proyectos de Kotlin!

## Ver también

- Documentación de SnakeYAML: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- Ejemplos de YAML en Kotlin: https://github.com/damyanbogoev/yaml-kotlin-examples