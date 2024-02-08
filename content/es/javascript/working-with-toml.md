---
title:                "Trabajando con TOML"
aliases:
- es/javascript/working-with-toml.md
date:                  2024-01-26T04:23:17.077850-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/working-with-toml.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
TOML, abreviatura de Tom's Obvious, Minimal Language, define cómo estructurar archivos de configuración. Los programadores trabajan con TOML porque es fácil de leer, escribir y se mapea bien a una tabla hash, lo que lo convierte en una opción predilecta para las configuraciones.

## Cómo hacerlo:
Para trabajar con TOML en JavaScript, necesitarás un analizador como `@iarna/toml`. Primero, instálalo: `npm install @iarna/toml`. Luego, analiza una cadena TOML a un objeto JavaScript o convierte un objeto JavaScript a formato TOML.

```javascript
const toml = require('@iarna/toml');

// Analiza la cadena TOML a objeto JS
const tomlStr = `
title = "Ejemplo TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Convierte objeto JS a cadena TOML
const jsObject = {
  title: "Ejemplo TOML",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Análisis Profundo
TOML fue lanzado por primera vez en 2013 por Tom Preston-Werner, un cofundador de GitHub. Fue diseñado para superar a otros formatos, como INI, al ser más estandarizado y fácil de analizar. JSON y YAML son alternativas pero pueden ser demasiado complejos o demasiado flexibles. La ventaja de TOML está en la configuración estática donde se prefiere un formato simple y claro. Su diseño permite un mapeo directo a una tabla hash, con claves y valores correspondientes a nombres de propiedades y sus valores. Para una adopción más amplia, es posible que necesites integrar herramientas que puedan convertir entre TOML y otros formatos debido al variado soporte del ecosistema.

## Ver También
- El repositorio oficial de TOML en GitHub: https://github.com/toml-lang/toml
- Comparación entre TOML vs. YAML vs. JSON: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- Paquete npm `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
