---
title:                "Trabajando con YAML"
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Trabajar con YAML significa manejar datos en un formato legible por humanos, muy usado en configuraciones y datos de entrada. Los programadores lo usan por su simplicidad y claridad especialmente en proyectos que involucran Docker, Kubernetes o cualquier desarrollo de software que requiera configuraciones estructuradas.

## Cómo se hace:

Aquí tienes cómo leer y escribir YAML con JavaScript. Necesitarás una librería como `js-yaml`.

```javascript
// Primero, instala js-yaml con: npm install js-yaml

// Importa la librería
const yaml = require('js-yaml');
const fs   = require('fs');

// Para cargar un archivo YAML
try {
  const doc = yaml.load(fs.readFileSync('config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.log(e);
}

// Para guardar datos en formato YAML
let data = {
  title: 'Ejemplo',
  description: 'Un objeto YAML simple.'
};

try {
  let yamlStr = yaml.dump(data);
  fs.writeFileSync('config.yaml', yamlStr, 'utf8');
  console.log('YAML guardado!');
} catch (e) {
 console.log(e);
}
```

El output será el contenido del YAML cargado y un mensaje de confirmación al guardar.

## Profundización

YAML, que significa "YAML Ain't Markup Language", fue creado en 2001 para ser una alternativa más humana al XML. Aunque JSON es más popular para APIs, YAML sigue siendo preferido en configuraciones debido a su fácil lectura. Al trabajar con YAML en JavaScript, la implementación más común es la librería `js-yaml`. Otras opciones incluyen `yamljs`. Cada una tiene su propia forma de manejar la especificación de YAML, con `js-yaml` ofreciendo una amplia compatibilidad con YAML 1.2.

## Vea también

- Documentación de `js-yaml`: [https://github.com/nodeca/js-yaml](https://github.com/nodeca/js-yaml)
- Especificación oficial de YAML: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Un tutorial interactivo de YAML: [https://learnxinyminutes.com/docs/yaml/](https://learnxinyminutes.com/docs/yaml/)
