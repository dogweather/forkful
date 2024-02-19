---
aliases:
- /es/javascript/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:35.493084-07:00
description: "YAML, abreviatura de YAML Ain't Markup Language (YAML No es un Lenguaje\
  \ de Marcado), es un formato de serializaci\xF3n de datos legible por humanos. Los\u2026"
lastmod: 2024-02-18 23:09:10.427373
model: gpt-4-0125-preview
summary: "YAML, abreviatura de YAML Ain't Markup Language (YAML No es un Lenguaje\
  \ de Marcado), es un formato de serializaci\xF3n de datos legible por humanos. Los\u2026"
title: Trabajando con YAML
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

YAML, abreviatura de YAML Ain't Markup Language (YAML No es un Lenguaje de Marcado), es un formato de serialización de datos legible por humanos. Los programadores a menudo lo usan para archivos de configuración e intercambio de datos entre lenguajes debido a su simplicidad y legibilidad en comparación con JSON o XML.

## Cómo hacerlo:

En JavaScript, trabajar con YAML implica típicamente el uso de una biblioteca de terceros, ya que el lenguaje no incluye un analizador integrado para YAML. Una de las bibliotecas más populares para este propósito es `js-yaml`. Puedes usar `js-yaml` para analizar YAML en objetos de JavaScript y viceversa.

Primero, necesitas instalar `js-yaml`:

```bash
npm install js-yaml
```

Luego, puedes usarlo en tus proyectos. Así es cómo puedes cargar un archivo YAML y analizarlo en un objeto de JavaScript:

```javascript
// Requiere el módulo js-yaml
const yaml = require('js-yaml');
const fs   = require('fs');

// Carga YAML de un archivo
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Si tu archivo `config.yaml` se ve así:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

La salida será:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

Para hacer lo contrario, convirtiendo un objeto de JavaScript en una cadena YAML:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Este código producirá:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

Usando `js-yaml`, puedes integrar fácilmente el análisis y la serialización de YAML en tus proyectos de JavaScript, mejorando la intercambiabilidad de datos y la gestión de configuraciones.
