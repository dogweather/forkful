---
title:                "Trabajando con YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
YAML es un formato para guardar datos de forma legible, usado en configuración y transmisión de información entre servicios. Los programadores lo usan por su simplicidad y facilidad para ser entendido tanto por humanos como por máquinas.

## Cómo Hacerlo:
Para trabajar con YAML en Kotlin, se puede usar la librería `snakeyaml`. Primero, añade la dependencia en tu archivo `build.gradle`:

```kotlin
dependencies {
    implementation("org.yaml:snakeyaml:1.29")
}
```

A continuación, un ejemplo básico de cómo leer y escribir YAML:

```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream
import java.io.FileWriter

// Escribir YAML
val data = mapOf("nombre" to "Juan", "edad" to 30)
val yaml = Yaml()
val writer = FileWriter("datos.yaml")
yaml.dump(data, writer)

// Leer YAML
val inputStream = FileInputStream("datos.yaml")
val readData: Map<String, Any> = yaml.load(inputStream)
println(readData) // Muestra: {nombre=Juan, edad=30}
```

## Conozcamos Más:
El YAML comenzó en 2001 como un lenguaje de marcado más legible y fácil de usar que el XML. Alternativas incluyen JSON y XML, pero YAML es preferido para archivos de configuración debido a su claridad. Su implementación en Kotlin es mediante librerías de terceros como `snakeyaml`, que internamente convierte datos YAML a objetos de Kotlin y viceversa.

## Ver También:
- Manual oficial de YAML: https://yaml.org/spec/1.2/spec.html
- Librería Snakeyaml en GitHub: https://github.com/asomov/snakeyaml
- Guía de YAML para principiantes: https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started
