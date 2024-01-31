---
title:                "Trabajando con JSON"
date:                  2024-01-19
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Trabajamos con JSON porque es un formato liviano de intercambio de datos, fácil de leer para humanos y simple de analizar para máquinas. Es esencial en la web para enviar datos entre el cliente y el servidor.

## Cómo hacerlo:

```TypeScript
import * as fs from 'fs';

// Definir una interfaz para el tipo de datos
interface Usuario {
  id: number;
  nombre: string;
  email: string;
}

// Leer y parsear un archivo JSON
const datosCrudos = fs.readFileSync('usuarios.json', 'utf-8');
const usuarios: Usuario[] = JSON.parse(datosCrudos);

// Mostrar los usuarios
console.log(usuarios);

// Crear un objeto usuario y convertirlo a JSON
const nuevoUsuario: Usuario = { id: 3, nombre: 'Ana', email: 'ana@ejemplo.com' };
const usuarioJson = JSON.stringify(nuevoUsuario);

// Escribir el JSON a un archivo
fs.writeFileSync('nuevo-usuario.json', usuarioJson);
```

Salida de ejemplo al ejecutar el código (contenido del archivo 'usuarios.json'):

```TypeScript
[
  { "id": 1, "nombre": "Juan", "email": "juan@ejemplo.com" },
  { "id": 2, "nombre": "Marta", "email": "marta@ejemplo.com" }
]
```

## Profundización

JSON (JavaScript Object Notation) nació a principios de los 2000 y se derivó de la notación de objeto de JavaScript. Antes de JSON, XML era el rey para intercambiar datos, pero era más pesado y complicado de analizar. Otras alternativas incluyen YAML, más legible pero menos universal. Los detalles clave en el manejo de JSON en TypeScript incluyen definir interfaces para tipado estático y usar `JSON.parse()` y `JSON.stringify()` para la conversión de/para string.

## Ver También

- [MDN Web Docs – JSON](https://developer.mozilla.org/es/docs/Learn/JavaScript/Objects/JSON)
- [TypeScript: Documentación oficial](https://www.typescriptlang.org/docs/)
- [JSON.org](https://www.json.org/json-es.html)
