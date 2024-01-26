---
title:                "Trabajando con XML"
date:                  2024-01-26T04:36:06.547868-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con XML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/working-with-xml.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Trabajar con XML significa analizar, manipular y escribir datos XML mediante programación. Los programadores manejan XML para intercambiar datos a través de diferentes sistemas, para archivos de configuración, o cuando se trabaja con estándares como SOAP que dependen de XML.

## Cómo hacerlo:
```TypeScript
import { parseString } from 'xml2js';

// Ejemplo de XML
const xml = `<note>
                <to>Usuario</to>
                <from>Autor</from>
                <heading>Recordatorio</heading>
                <body>¡No olvides la reunión!</body>
             </note>`;

// Analizar XML a JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// Asumiendo que el análisis fue exitoso, la salida podría parecerse a:
// { note:
//    { to: ['Usuario'],
//      from: ['Autor'],
//      heading: ['Recordatorio'],
//      body: ['¡No olvides la reunión!'] } 
}
```

## Profundización
XML, o Lenguaje de Marcado Extensible, existe desde fines de los '90. Su naturaleza autodescriptiva y formato legible por humanos lo hicieron popular desde el principio para diversas aplicaciones como fuentes RSS, gestión de configuración, e incluso formatos de documentos de oficina como Microsoft Office Open XML. Pero, es verboso comparado con JSON, y la tendencia ha cambiado. JSON ha ganado protagonismo para APIs basadas en la web debido a su menor peso y compatibilidad nativa con JavaScript.

No obstante, XML no ha muerto. Se utiliza en sistemas empresariales a gran escala y para estándares de documentos que no han cambiado a JSON. Herramientas como `xml2js` para TypeScript o `lxml` en Python prueban que hay una necesidad continua de manipulación de XML en la programación.

TypeScript no tiene soporte incorporado para XML como lo tiene para JSON. En cambio, se trabaja con bibliotecas. `xml2js` es un ejemplo. Transforma XML en JSON, facilitando el manejo de los datos para los gurús de JavaScript.

## Ver También
- [Documentos Web MDN sobre XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [Paquete npm xml2js](https://www.npmjs.com/package/xml2js)
- [Tutorial XML de W3Schools](https://www.w3schools.com/xml/)