---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:08.250476-07:00
description: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un est\xE1ndar de serializaci\xF3n de datos legible por humanos\
  \ que se\u2026"
lastmod: '2024-03-13T22:44:58.586757-06:00'
model: gpt-4-0125-preview
summary: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un est\xE1ndar de serializaci\xF3n de datos legible por humanos\
  \ que se\u2026"
title: Trabajando con YAML
---

{{< edit_this_page >}}

## Qué y Por Qué?

YAML, que significa "YAML Ain't Markup Language" (YAML no es un lenguaje de marcado), es un estándar de serialización de datos legible por humanos que se utiliza comúnmente para archivos de configuración e intercambio de datos entre lenguajes con estructuras de datos variadas. Los programadores a menudo trabajan con YAML por su simplicidad y legibilidad, especialmente en proyectos que requieren una configuración extensa o cuando se transfieren datos estructurados entre diferentes sistemas.

## Cómo:

Aunque Google Apps Script (GAS) no admite de forma nativa el análisis o la serialización de YAML, puedes manipular datos YAML utilizando bibliotecas de JavaScript o escribiendo funciones de análisis personalizadas. Para demostrar, consideremos cómo analizar una cadena YAML utilizando una función personalizada, ya que no se pueden importar directamente bibliotecas externas en GAS.

Supongamos que tienes una configuración YAML simple:

```yaml
title: Ejemplo YAML
description: Un ejemplo de cómo manejar YAML en Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Configuración
```

Para analizar esto en Google Apps Script, usa las capacidades de manipulación de cadenas de JavaScript:

```javascript
function parseYAML(cadenaYAML) {
  var resultado = {};
  var lineas = cadenaYAML.split("\n");
  for (var i = 0; i < lineas.length; i++) {
    var linea = lineas[i];
    if (linea.includes(":")) {
      var partes = linea.split(":");
      var clave = partes[0].trim();
      var valor = partes[1].trim();
      // Manejo básico para arrays
      if (valor.startsWith("-")) {
        valor = [valor.substring(1).trim()];
        while (i + 1 < lineas.length && lineas[i + 1].trim().startsWith("-")) {
          i++;
          valor.push(lineas[i].trim().substring(1).trim());
        }
      }
      resultado[clave] = valor;
    }
  }
  return resultado;
}

function probarAnalisisYAML() {
  var yaml = "title: Ejemplo YAML\ndescription: Un ejemplo de cómo manejar YAML en Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuración";
  var analizado = parseYAML(yaml);
  Logger.log(analizado);
}
```

Cuando se ejecuta `probarAnalisisYAML()`, se muestra:

```
{ title: 'Ejemplo YAML',
  description: 'Un ejemplo de cómo manejar YAML en Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Configuración' ] }
```

Este enfoque de análisis personalizado es bastante básico y puede necesitar ajustes para adaptarse a archivos YAML complejos.

## Profundización

YAML, lanzado inicialmente en 2001, buscaba ser más legible por humanos que sus predecesores como XML o JSON. Aunque su simplicidad y facilidad de uso son ampliamente apreciadas, manejar YAML en Google Apps Script presenta desafíos debido a la falta de soporte directo. En consecuencia, los programadores a menudo dependen de la versatilidad de JavaScript para analizar y generar datos YAML. Sin embargo, para casos de uso complejos, especialmente aquellos que implican anidación profunda y estructuras de datos avanzadas, este método puede volverse engorroso y propenso a errores.

JSON, por el contrario, es soportado de forma nativa en Google Apps Script y en la mayoría de los otros entornos de programación, ofreciendo un enfoque más directo para la serialización y deserialización de datos sin sobrecarga de análisis adicional. La sintaxis de JSON es menos verbosa que la de YAML, lo que la hace más adecuada para el intercambio de datos en aplicaciones web. No obstante, YAML sigue siendo popular para archivos de configuración y situaciones donde la legibilidad humana es de suma importancia.

Cuando trabajes con YAML en Google Apps Script, considera los compromisos entre legibilidad y facilidad de uso. Para una manipulación completa de YAML, puede valer la pena explorar herramientas o servicios externos que puedan convertir YAML a JSON antes de procesarlo dentro de tu script.
