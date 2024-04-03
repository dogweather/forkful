---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:52.621539-07:00
description: "TOML, que significa \"Tom's Obvious, Minimal Language\" (Lenguaje Obvio\
  \ y M\xEDnimo de Tom), es un formato de archivo de configuraci\xF3n f\xE1cil de\
  \ leer debido a\u2026"
lastmod: '2024-03-13T22:44:58.590572-06:00'
model: gpt-4-0125-preview
summary: "TOML, que significa \"Tom's Obvious, Minimal Language\" (Lenguaje Obvio\
  \ y M\xEDnimo de Tom), es un formato de archivo de configuraci\xF3n f\xE1cil de\
  \ leer debido a su sem\xE1ntica clara."
title: Trabajando con TOML
weight: 39
---

## ¿Qué y por qué?

TOML, que significa "Tom's Obvious, Minimal Language" (Lenguaje Obvio y Mínimo de Tom), es un formato de archivo de configuración fácil de leer debido a su semántica clara. Los programadores a menudo lo utilizan para archivos de configuración en aplicaciones porque es directo y legible por humanos, lo que facilita la gestión de configuraciones y ajustes de la aplicación sin problemas a través de diferentes entornos.

## Cómo hacerlo:

Dado que Google Apps Script es esencialmente JavaScript con acceso al conjunto de aplicaciones de Google, trabajar con TOML directamente dentro de Google Apps Script requiere un poco de ingenio. Google Apps Script no admite de forma nativa el análisis de TOML, pero puedes aprovechar las bibliotecas de JavaScript o escribir un analizador simple para necesidades básicas.

Vamos a analizar una cadena de configuración TOML simple como ejemplo:

```javascript
// Cadena TOML
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// Función de análisis de TOML a JSON simple
function parseTOML(tomlStr) {
  var resultado = {};
  var secciónActual = resultado;
  tomlStr.split(/\r?\n/).forEach(linea => {
    linea = linea.trim();
    if (linea.startsWith('[')) { // Nueva sección
      var nombreSección = linea.replace(/\[|\]/g, '');
      resultado[nombreSección] = {};
      secciónActual = resultado[nombreSección];
    } else if (linea) {
      var claveValor = linea.split('=').map(parte => parte.trim());
      var clave = claveValor[0];
      var valor = eval(claveValor[1]); // Usar eval por simplicidad; tener cuidado en código de producción
      secciónActual[clave] = valor;
    }
  });
  return resultado;
}

// Probar el analizador
var objetoConfig = parseTOML(tomlString);
console.log(objetoConfig);

```

La muestra de salida del `console.log` se parecería a un objeto JSON, lo que facilita acceder a las propiedades de configuración dentro de Google Apps Script:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## Análisis profundo

TOML fue creado por Tom Preston-Werner, uno de los fundadores de GitHub, para ser más amigable para humanos que JSON en archivos de configuración, manteniendo al mismo tiempo la capacidad de ser analizado de manera unívoca. Aspira a ser lo más simple posible, un objetivo que se alinea bien con el ethos de muchos proyectos de desarrollo que buscan simplicidad y legibilidad en sus bases de código.

En el contexto de Google Apps Script, usar TOML puede introducir cierta sobrecarga, dada la falta de soporte directo y la necesidad de analizarlo manualmente o mediante bibliotecas de terceros. Para proyectos más pequeños o aquellos no profundamente integrados en el ecosistema de Google, alternativas como JSON o incluso estructuras simples de pares clave-valor en propiedades de script podrían ser suficientes y más directas de implementar. Sin embargo, para aplicaciones que priorizan archivos de configuración amigables para humanos y ya están comprometidos con TOML, integrar el análisis de TOML a través de scripts personalizados agrega una capa útil de flexibilidad y mantenibilidad sin apartarse de los paradigmas de configuración preferidos.
