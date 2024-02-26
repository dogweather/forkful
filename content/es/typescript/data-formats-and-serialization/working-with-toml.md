---
date: 2024-01-26 04:26:56.836492-07:00
description: "TOML, abreviatura de Tom's Obvious, Minimal Language (Lenguaje M\xED\
  nimo y Obvio de Tom), es un formato de serializaci\xF3n de datos similar a JSON\
  \ o YAML. Los\u2026"
lastmod: '2024-02-25T18:49:55.322489-07:00'
model: gpt-4-0125-preview
summary: "TOML, abreviatura de Tom's Obvious, Minimal Language (Lenguaje M\xEDnimo\
  \ y Obvio de Tom), es un formato de serializaci\xF3n de datos similar a JSON o YAML.\
  \ Los\u2026"
title: Trabajando con TOML
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
TOML, abreviatura de Tom's Obvious, Minimal Language (Lenguaje Mínimo y Obvio de Tom), es un formato de serialización de datos similar a JSON o YAML. Los programadores lo utilizan por su legibilidad humana y su mapeo directo a tipos de datos, lo que lo convierte en una opción predilecta para archivos de configuración e intercambio de datos.

## Cómo hacerlo:
Primero, necesitarás un analizador de TOML. `@iarna/toml` es una opción popular. Instálalo con npm: `npm install @iarna/toml --save`. Aquí te mostramos cómo leer un archivo TOML y analizarlo a un objeto de JavaScript:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const contenidoToml = fs.readFileSync('config.toml', 'utf-8');
const datosAnalizados = toml.parse(contenidoToml);

console.log(datosAnalizados);
```
Si `config.toml` contiene:
```
[server]
port = 8080
```
La salida sería:
```
{ server: { port: 8080 } }
```
Y, escribir en un archivo TOML es igual de sencillo:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const stringToml = stringify(obj);
fs.writeFileSync('config.toml', stringToml);
``` 
Ejecutar este código escribe el objeto en `config.toml` en formato TOML.

## Análisis Profundo
TOML fue creado por Tom Preston-Werner, el co-fundador de GitHub, alrededor de 2013 como respuesta a las limitaciones que percibía en otros formatos como INI o YAML. Está diseñado para ser inequívoco y fácil de analizar en estructuras de datos, por lo tanto, es favorito para archivos de configuración. Alternativas como JSON carecen de comentarios, mientras que YAML es más complejo. TOML destaca por su simplicidad y su capacidad para representar claramente jerarquías de datos complejas.

Bajo el capó, cuando analizas TOML en TypeScript, estás convirtiendo datos textuales en un formato estructurado que el lenguaje puede manipular. Esto implica lexing (convertir texto crudo en tokens) y analizar (construir una estructura de datos interna); `@iarna/toml` maneja ambos sin problemas. El soporte de emojis es un toque divertido, mostrando el enfoque centrado en el usuario de TOML.

## Ver También
- Especificación Oficial de TOML: https://toml.io/en/
- Paquete `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
- Comparaciones entre TOML, YAML y JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
