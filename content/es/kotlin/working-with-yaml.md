---
title:                "Trabajando con yaml"
html_title:           "Kotlin: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con YAML es una forma de estructurar y almacenar datos en un formato legible para humanos y también para las máquinas. Los programadores utilizan YAML para crear configuraciones y definir estructuras de datos en sus proyectos.

## Cómo:

```kotlin 
// Ejemplo de un archivo YAML
animals: 
  - dog
  - cat
  - bird
  - fish
  - lion
```

La salida de este ejemplo sería una lista de animales que incluye perro, gato, pájaro, pez y león. Para trabajar con YAML en Kotlin, se puede utilizar la biblioteca SnakeYAML. A continuación se muestra un código de ejemplo para leer un archivo YAML y acceder a su contenido:

```kotlin
// Importar la biblioteca SnakeYAML
import org.yaml.snakeyaml.Yaml 

// Cargar el archivo YAML
val file = File("animales.yaml") 
val yaml = Yaml() 
val data = yaml.load(file.inputStream) 

// Acceder al contenido del archivo YAML
val animals = data.get("animals") 
for (animal in animals as List<String>){ 
    println(animal) 
} 
```

## Deep Dive:

YAML, que es un acrónimo de "YAML Ain't Markup Language", es un formato de serialización de datos introducido en 2001 como una alternativa ligera al formato XML. YAML es conocido por su simplicidad y legibilidad, lo que lo hace útil para almacenar configuraciones y definir estructuras de datos en proyectos de programación. Algunas alternativas a YAML incluyen JSON, XML y TOML. En Kotlin, se puede utilizar la biblioteca SnakeYAML para trabajar con YAML.

## See Also:

- [Página oficial de YAML](https://yaml.org)
- [Documentación de Kotlin sobre SnakeYAML](https://kotlinlang.org/docs/kotlin-eap-1.4.0.html#yaml)