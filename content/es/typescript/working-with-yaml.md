---
title:                "Trabajando con YAML"
date:                  2024-01-19
simple_title:         "Trabajando con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con YAML significa manipular y gestionar datos en el formato YAML ("YAML Ain't Markup Language"), un lenguaje de serialización legible por humanos. Programadores lo usan para configuraciones, archivos de datos y como manera de intercambiar información entre servicios y aplicaciones debido a su simplicidad y facilidad de lectura.

## Cómo hacerlo:

Para manejar YAML en TypeScript, primero añade una librería como `js-yaml`. Instálala vía npm:

```bash
npm install js-yaml
```

Usa la librería para convertir un objeto de TypeScript a YAML y viceversa:

```TypeScript
import * as yaml from 'js-yaml';

// Convertir objeto a YAML
const objeto = { nombre: "Juan", edad: 30, empleo: "Desarrollador" };
const yamlString = yaml.dump(objeto);
console.log(yamlString);

// Convertir YAML a objeto
const yamlCargado = yaml.load(`
nombre: Juan
edad: 30
empleo: Desarrollador
`);
console.log(yamlCargado);
```

Resultado de console.log para `yamlString`:

```yaml
nombre: Juan
edad: 30
empleo: Desarrollador
```

Resultado de console.log para `yamlCargado`:

```json
{ nombre: 'Juan', edad: 30, empleo: 'Desarrollador' }
```

## Profundización

YAML fue introducido en 2001 para ser más fácil de entender y usar que otros formatos de serialización como XML. Aunque JSON es otra alternativa popular, YAML es preferido en contextos donde la legibilidad humana es crítica. Al trabajar con TypeScript, el tipado estático ayuda a mantener la integridad de los datos cuando se serializan y deserializan. Detrás de escena, herramientas como `js-yaml` utilizan algoritmos de análisis (parsing) para convertir textos YAML en estructuras de datos y viceversa.

## Ver También

- Documentación oficial de YAML: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Repositorio npm de `js-yaml`: [https://www.npmjs.com/package/js-yaml](https://www.npmjs.com/package/js-yaml)
