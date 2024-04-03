---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:32.748909-07:00
description: "JSON, o Notaci\xF3n de Objeto de JavaScript, es un formato ligero para\
  \ almacenar y transportar datos, ideal para la comunicaci\xF3n servidor-cliente\
  \ y archivos\u2026"
lastmod: '2024-03-13T22:44:58.588006-06:00'
model: gpt-4-0125-preview
summary: "JSON, o Notaci\xF3n de Objeto de JavaScript, es un formato ligero para almacenar\
  \ y transportar datos, ideal para la comunicaci\xF3n servidor-cliente y archivos\
  \ de configuraci\xF3n."
title: Trabajando con JSON
weight: 38
---

## Qué y Por Qué?

JSON, o Notación de Objeto de JavaScript, es un formato ligero para almacenar y transportar datos, ideal para la comunicación servidor-cliente y archivos de configuración. Los programadores lo aprovechan en Google Apps Script para un intercambio de datos fluido entre servicios de Google (como Sheets, Docs, Drive) y fuentes externas, gracias a su estructura legible por humanos y fácil integración dentro de entornos basados en JavaScript.

## Cómo hacerlo:

En Google Apps Script, manipular JSON es un proceso sencillo, en gran parte debido al soporte nativo que JavaScript ofrece para el análisis (parsing) y la conversión a cadena de JSON (stringification). Aquí hay algunas operaciones comunes:

**1. Análisis de JSON**: Supongamos que recuperamos una cadena JSON de un servicio web; convertirla en un objeto JavaScript es esencial para la manipulación de datos.

```javascript
var jsonString = '{"name": "Proyecto de Muestra", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Salida: Proyecto de Muestra
```

**2. Convertir Objetos JavaScript a Cadenas JSON**: Por el contrario, convertir un objeto JavaScript a una cadena JSON es útil cuando necesitamos enviar datos desde Apps Script a un servicio externo.

```javascript
var projectData = {
  name: "Proyecto de Muestra",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Salida: '{"name":"Proyecto de Muestra","version":"1.0.0"}'
```

**3. Trabajar con Datos Complejos**:
Para estructuras de datos más complejas, como arrays de objetos, el proceso permanece igual, mostrando la flexibilidad de JSON para la representación de datos.

```javascript
var projects = [
  {name: "Proyecto 1", version: "1.0"},
  {name: "Proyecto 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // Salida: '[{"name":"Proyecto 1","version":"1.0"},{"name":"Proyecto 2","version":"2.0"}]'
```

## Profundización

La ubicuidad de JSON en aplicaciones web modernas no se puede subestimar, arraigada en su simplicidad y cómo se integra a la perfección con JavaScript, el lenguaje de la web. Su diseño, inspirado en literales de objeto de JavaScript, aunque más estricto, facilita su rápida adopción. A principios de los 2000, JSON ganó popularidad como una alternativa a XML para aplicaciones web impulsadas por AJAX, ofreciendo un formato de intercambio de datos más ligero y menos verboso. Dada la profunda integración de Google Apps Script con varias APIs de Google y servicios externos, JSON sirve como un formato clave para estructurar, transportar y manipular datos a través de estas plataformas.

Aunque JSON reina supremo para aplicaciones web, existen formatos de datos alternativos como YAML para archivos de configuración o Protobuf para una serialización binaria más eficiente en entornos de alto rendimiento. Sin embargo, el equilibrio de JSON entre legibilidad, facilidad de uso y amplio soporte a través de lenguajes de programación y herramientas solidifica su posición como la elección predeterminada para muchos desarrolladores incursionando en Google Apps Script y más allá.
