---
title:                "Trabajando con XML"
aliases:
- es/fish-shell/working-with-xml.md
date:                  2024-01-26T04:30:02.801762-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/working-with-xml.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Trabajar con XML significa manipular datos en un formato estructurado y omnipresente utilizado en configuraciones, mensajería y más. Los programadores manipulan XML para leer, escribir, actualizar y consultar datos, vital para la interoperabilidad en toneladas de aplicaciones y servicios.

## Cómo hacerlo:
Fish no tiene análisis de XML incorporado, así que dependerás de herramientas externas como `xmllint` o `xmlstarlet`. Aquí hay un fragmento para leer valores:

```fish
# Analizar XML usando xmlstarlet
echo '<root><element>Hola Mundo</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Salida:
```
Hola Mundo
```

Para editar XML, usa esto:

```fish
# Editar elemento XML utilizando xmlstarlet
echo '<root><element>Valor Antiguo</element></root>' | xmlstarlet ed -u "/root/element" -v 'Nuevo Valor'
```

Salida:
```xml
<?xml version="1.0"?>
<root>
  <element>Nuevo Valor</element>
</root>
```

## Estudio Profundo:
XML existe desde finales de los '90, diseñado para ser legible y agradable para la máquina. Aunque la simplicidad de JSON ha usurpado parte de la popularidad de XML, este último permanece arraigado donde la validación de documentos y los espacios de nombres son clave.

¿Alternativas? Seguro: JSON, YAML o incluso formatos binarios como Protocol Buffers para aquellas aplicaciones intensivas en rendimiento. Pero el esquema de XML y XSLT (para transformaciones de XML) pueden ser decisivos en escenarios complejos donde la robustez es importante.

Bajo el capó, herramientas como `xmlstarlet` envuelven librerías poderosas como libxml2, otorgándote XPath y XQuery para ajustes finos en XML. Estas no son solo herramientas de XML, sino puertas de acceso a la manipulación del DOM, ya que aplicarías conceptos similares en cualquier lenguaje que toque XML.

## Ver También:
- [Documentación de xmlstarlet](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Documentación de Fish](https://fishshell.com/docs/current/index.html)
- [Funciones y Operadores de XPath y XQuery](https://www.w3.org/TR/xpath-functions/)
