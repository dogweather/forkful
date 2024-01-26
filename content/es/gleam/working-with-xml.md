---
title:                "Trabajando con XML"
date:                  2024-01-26T04:30:57.722252-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con XML implica parsear, manipular y generar documentos XML, los cuales se utilizan para el intercambio de datos debido a su formato estructurado y ampliamente difundido. Los programadores manejan XML para interactuar con innumerables sistemas donde el XML es la lengua franca de los datos.

## Cómo hacerlo:
Gleam no soporta nativamente XML, así que utilizaremos una biblioteca externa como `gleam_xml`. Primero, agrégala a tu `gleam.toml`:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Ahora, para parsear y crear XML:

```rust
import gleam/xml

// Parsear XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Crear XML
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")]),
  ]
)
let xml_string = xml.render(node)
```

La salida de muestra para `xml.render(node)` es:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Profundizando
XML significa eXtensible Markup Language, una especificación del W3C como hermana del HTML. Ha existido desde fines de los años 90. Para Gleam, manejar XML se siente un poco como un paso atrás en el tiempo. JSON y Protocol Buffers están más de moda, pero el uso extensivo del XML en sistemas legados y ciertas industrias significa que todavía es relevante.

Existen alternativas como `xmerl` en el ecosistema de Erlang; sin embargo, la biblioteca `gleam_xml` proporciona un enfoque más idiomático para los usuarios de Gleam. Está construida sobre bibliotecas de Erlang existentes pero expone una API amigable para Gleam. El enfoque de Gleam hacia el XML apunta a la simplicidad y seguridad, reduciendo el código innecesario y enfatizando la seguridad de tipos.

En cuanto a la implementación, las bibliotecas de XML, incluyendo `gleam_xml`, típicamente proporcionan estructuras similares al DOM. Esto implica nodos, atributos y elementos anidados, aprovechando el modelado de patrones y los modelos de concurrencia de Erlang para manejar documentos potencialmente grandes y complejos.

## Ver también
- La biblioteca `gleam_xml` en Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Norma oficial de XML por W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Tutorial completo de XML: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Documentación de `xmerl` de Erlang para el procesamiento de XML: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)