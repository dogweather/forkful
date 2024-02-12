---
title:                "Trabajando con YAML"
aliases:
- /es/typescript/working-with-yaml.md
date:                  2024-02-03T19:27:36.847329-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
YAML, un lenguaje de serialización de datos diseñado para ser amigable con el humano, se usa a menudo para archivos de configuración, mensajes entre procesos y almacenamiento de datos. Los programadores se apoyan en YAML por su legibilidad y facilidad de uso, especialmente cuando se trata de datos estructurados complejos, lo que lo hace una excelente elección para aplicaciones desarrolladas en TypeScript.

## Cómo hacerlo:
Trabajar con YAML en TypeScript típicamente involucra parsear el contenido YAML en objetos de JavaScript y posiblemente convertir objetos de JavaScript de vuelta a YAML. Esto requiere un analizador; una opción popular es `js-yaml`, una biblioteca que puede ser fácilmente integrada en proyectos TypeScript.

### Instalando js-yaml
Primero, añade `js-yaml` a tu proyecto:

```bash
npm install js-yaml
```

### Parseando YAML a Objeto JavaScript
Imagina que tienes un archivo YAML `config.yaml` con el siguiente contenido:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

Puedes leer y parsear este archivo en un objeto JavaScript de la siguiente manera:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// Carga y parsea el archivo YAML
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**Salida de Ejemplo:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### Convirtiendo Objeto JavaScript a YAML
Si necesitas hacer lo contrario y convertir un objeto JavaScript a una cadena YAML, puedes usar `js-yaml` de la siguiente manera:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**Salida de Ejemplo:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

Este fragmento convierte un objeto JavaScript a una cadena YAML y la muestra. En la práctica, podrías escribir esto de vuelta a un archivo o usarlo en otras partes de tu aplicación.
