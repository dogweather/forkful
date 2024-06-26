---
date: 2024-01-26 04:22:52.918349-07:00
description: "C\xF3mo hacerlo: Necesitar\xE1s una biblioteca de an\xE1lisis de TOML.\
  \ Recomiendo `toml4j`. Agr\xE9gala a tu proyecto de esta manera."
lastmod: '2024-03-13T22:44:58.960828-06:00'
model: gpt-4-0125-preview
summary: "Necesitar\xE1s una biblioteca de an\xE1lisis de TOML."
title: Trabajando con TOML
weight: 39
---

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
