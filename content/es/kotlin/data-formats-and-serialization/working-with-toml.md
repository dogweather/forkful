---
date: 2024-01-26 04:23:36.940357-07:00
description: "TOML significa Tom's Obvious, Minimal Language (Lenguaje M\xEDnimo y\
  \ Obvio de Tom). Se utiliza para archivos de configuraci\xF3n porque es f\xE1cil\
  \ de leer y\u2026"
lastmod: '2024-03-13T22:44:59.059860-06:00'
model: gpt-4-0125-preview
summary: "TOML significa Tom's Obvious, Minimal Language (Lenguaje M\xEDnimo y Obvio\
  \ de Tom). Se utiliza para archivos de configuraci\xF3n porque es f\xE1cil de leer\
  \ y\u2026"
title: Trabajando con TOML
---

{{< edit_this_page >}}

## Qué y Por Qué?
TOML significa Tom's Obvious, Minimal Language (Lenguaje Mínimo y Obvio de Tom). Se utiliza para archivos de configuración porque es fácil de leer y escribir para los humanos, mientras sigue siendo fácil de analizar para las máquinas. Los desarrolladores optan por TOML para evitar el desorden de XML y la complejidad de JSON cuando manipulan configuraciones.

## Cómo hacerlo:
Para manejar TOML en Kotlin, podrías usar una biblioteca como `ktoml`. Primero, vamos a agregar la dependencia en tu `build.gradle.kts`:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Ahora, vamos a analizar algo de TOML:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val contenidoToml = TomlFileReader.readAndParseFile("config.toml")
    
    val configuracionBaseDatos = contenidoToml.getTable("database")
    val host = configuracionBaseDatos.getString("host")
    val puerto = configuracionBaseDatos.getLong("port")

    println("Host de la Base de Datos: $host")
    println("Puerto de la Base de Datos: $puerto")
}
```

Asumiendo que `config.toml` se ve así:

```toml
[database]
host = "localhost"
port = 5432
```

El resultado sería:

```
Host de la Base de Datos: localhost
Puerto de la Base de Datos: 5432
```

## Análisis Profundo
TOML, ideado por el cofundador de GitHub, Tom Preston-Werner, en 2013, aspiraba a ser más sencillo que YAML y más seguro en cuanto a tipos que JSON. Se ha convertido en un éxito, especialmente con el `Cargo` de Rust y el sistema de módulos de Go. ¿Alternativas? YAML tiene más características, JSON se traduce directamente en objetos en muchos lenguajes de programación, y siempre está el buen y viejo XML. En cuanto a la implementación, ktoml, bajo la licencia Apache 2.0, es una biblioteca puramente Kotlin y no arrastra librerías de Java, ofreciendo DSLs para escribir TOML también, no solo para leer.

## Ver También
- El GitHub de TOML: https://github.com/toml-lang/toml
- El GitHub de ktoml: https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
