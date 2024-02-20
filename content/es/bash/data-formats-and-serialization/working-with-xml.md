---
date: 2024-01-26 04:27:22.475611-07:00
description: "Trabajar con XML implica analizar, extraer y manipular datos en el formato\
  \ de Lenguaje de Marcado Extensible. Los programadores luchan con XML ya que es\u2026"
lastmod: 2024-02-19 22:05:17.778177
model: gpt-4-0125-preview
summary: "Trabajar con XML implica analizar, extraer y manipular datos en el formato\
  \ de Lenguaje de Marcado Extensible. Los programadores luchan con XML ya que es\u2026"
title: Trabajando con XML
---

{{< edit_this_page >}}

## Qué y Por Qué?
Trabajar con XML implica analizar, extraer y manipular datos en el formato de Lenguaje de Marcado Extensible. Los programadores luchan con XML ya que es un formato de intercambio de datos muy extendido para configuraciones, APIs y más.

## Cómo hacerlo:
Aquí te mostramos cómo analizar XML en Bash. ¿Herramientas? xmllint y xmlstarlet. ¿Iterar a través de los elementos XML? Definitivamente. Ejemplo con salida de muestra:

```bash
# Asumiendo que xmlstarlet está instalado
# Instalar con: apt-get install xmlstarlet

# Analizando contenido XML
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# Extraer nombres con xmlstarlet
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# La salida debería ser:
# Apple
# Banana
```

## Profundización
A principios de los '90, XML apareció como una alternativa más simple a SGML, pero más estructurada que HTML. Ahora, tiene compañía - JSON, YAML, por ejemplo. Pero XML todavía está en la lucha, especialmente en configuraciones y servicios web basados en SOAP.

En términos de herramientas, xmllint es cómodo para la validación de XML, consultas xpath. xmlstarlet es la navaja suiza para las travesuras XML - consultar, editar, validar, transformar. En scripts de bash, son superhéroes para las tareas XML.

Bajo el capó, xmllint utiliza libxml2 - el parser C de XML. Es rápido, pero ¿los mensajes de error? Crípticos. ¿Y xmlstarlet? Plantillas recursivas y el soporte de EXSLT. Difícil de entender, pero poderoso.

## Ver También
- [xmlsoft.org](http://xmlsoft.org/): Cosas de Libxml2 y xmllint.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Problemas y soluciones del mundo real.
- [Tutorial XML de W3Schools](https://www.w3schools.com/xml/): Fundamentos de XML.
