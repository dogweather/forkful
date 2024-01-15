---
title:                "Trabajando con Yaml"
html_title:           "Kotlin: Trabajando con Yaml"
simple_title:         "Trabajando con Yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué

¿Te has encontrado con archivos de configuración que utilizan la sintaxis YAML y te preguntas qué es y cómo puedes trabajar con ella? Bueno, ¡has venido al lugar correcto! En este artículo, aprenderemos qué es YAML y cómo puede ser útil para los programadores.

## Cómo utilizar YAML en Kotlin

Primero, debemos agregar una dependencia en nuestro archivo `build.gradle` para poder utilizar la biblioteca de YAML en nuestro proyecto:

```Kotlin
dependencies {
    implementation("org.yaml:snakeyaml:1.27")
}
```

Luego, podemos utilizar la biblioteca en nuestro código de la siguiente manera:

```Kotlin
import org.yaml.snakeyaml.Yaml
import java.io.File

val file = File("archivo.yaml")
val yaml = Yaml()
val contenido = yaml.load(file.inputStream())

```

En este ejemplo, cargamos el contenido de un archivo YAML en un objeto de mapa de Kotlin. Ahora podemos acceder a los valores del mapa utilizando las claves correspondientes:

```Kotlin
val nombre = contenido["nombre"]
val edad = contenido["edad"]

println("¡Hola $nombre! Tengo $edad años.")
```

La salida de este código sería:

```
¡Hola María! Tengo 25 años.
```

## Inmersión profunda

Además de cargar archivos YAML en mapas de Kotlin, también podemos convertir objetos de Kotlin en cadenas YAML utilizando la biblioteca SnakeYAML:

```Kotlin
class Persona(val nombre: String, val edad: Int)

val persona = Persona("Juan", 30)
val yaml = Yaml()
val contenido = yaml.dump(persona)

println(contenido)
```

La salida de este código sería:

```
!!Persona
nombre: Juan
edad: 30
```

¡Ahora tienes una idea de lo útil que puede ser YAML en tu trabajo! Pero esto solo es el comienzo, hay muchas más funcionalidades y opciones disponibles. ¡Investiga y sigue aprendiendo para aprovechar al máximo YAML en tu trabajo diario!

## Véase también

- [Documentación oficial de YAML](https://yaml.org/spec/1.2/spec.html)
- [Repositorio de la biblioteca SnakeYAML](https://bitbucket.org/asomov/snakeyaml/wiki/Home)