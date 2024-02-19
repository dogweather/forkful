---
aliases:
- /es/java/working-with-toml/
date: 2024-01-26 04:22:52.918349-07:00
description: "TOML significa Tom's Obvious, Minimal Language (Lenguaje M\xEDnimo y\
  \ Obvio de Tom). Es un formato de serializaci\xF3n de datos utilizado para archivos\
  \ de\u2026"
lastmod: 2024-02-18 23:09:09.870426
model: gpt-4-0125-preview
summary: "TOML significa Tom's Obvious, Minimal Language (Lenguaje M\xEDnimo y Obvio\
  \ de Tom). Es un formato de serializaci\xF3n de datos utilizado para archivos de\u2026"
title: Trabajando con TOML
---

{{< edit_this_page >}}

## Qué y Por Qué?
TOML significa Tom's Obvious, Minimal Language (Lenguaje Mínimo y Obvio de Tom). Es un formato de serialización de datos utilizado para archivos de configuración. Los programadores lo usan porque es fácil de leer, escribir y se mapea bien a una tabla hash.

## Cómo hacerlo:
Necesitarás una biblioteca de análisis de TOML. Recomiendo `toml4j`. Agrégala a tu proyecto de esta manera:

```java
// Añade esto a tu build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Así es cómo se analiza un archivo TOML:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            puerto = 80
            """);

        String ip = toml.getString("server.ip");
        Integer puerto = toml.getLong("server.port").intValue();
        
        System.out.println("IP del Servidor: " + ip);
        System.out.println("Puerto del Servidor: " + puerto);
    }
}
```

Salida de muestra:

```
IP del Servidor: 192.168.1.1
Puerto del Servidor: 80
```

## Análisis Profundo
Desarrollado por el cofundador de GitHub, Tom Preston-Werner, TOML tenía como objetivo ser más simple que XML y más especificado que YAML. Su última versión 1.0.0, lanzada en 2021, ofrece un conjunto estable de características.

Alternativas como JSON o YAML también son populares. JSON es excelente para el intercambio de datos. YAML es más legible para configuraciones complejas. La fortaleza de TOML es su sencillez y su uso en la comunidad de Rust.

En cuanto a la implementación, cuando se usa TOML con Java, ten en cuenta que el analizador que elijas importa. Más allá de `toml4j`, algunos optan por `jackson-dataformat-toml`. Cada uno tendrá matices, como el manejo de errores o el rendimiento del análisis, así que elige en función de las necesidades de tu proyecto.

## Ver También
- Especificación de TOML: https://toml.io/es/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
