---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:19.688043-07:00
description: "C\xF3mo hacerlo: Google Apps Script, un lenguaje de scripting basado\
  \ en la nube para automatizar tareas a trav\xE9s de los productos de Google, no\
  \ tiene una\u2026"
lastmod: '2024-03-13T22:44:58.532694-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, un lenguaje de scripting basado en la nube para automatizar\
  \ tareas a trav\xE9s de los productos de Google, no tiene una herramienta REPL integrada\
  \ similar a las de lenguajes como Python o Node.js de JavaScript."
title: Usando un shell interactivo (REPL)
weight: 34
---

## Cómo hacerlo:
Google Apps Script, un lenguaje de scripting basado en la nube para automatizar tareas a través de los productos de Google, no tiene una herramienta REPL integrada similar a las de lenguajes como Python o Node.js de JavaScript. Sin embargo, puedes simular una experiencia similar utilizando las características de registro y depuración del Editor de Apps Script o configurando un entorno externo. Aquí, nos enfocamos en crear un REPL improvisado dentro del editor de Apps Script.

1. **Creando una función REPL improvisada**:

```javascript
function myREPL() {
  var input = Logger.log('Ingresa tu expresión: ');
  try {
    var result = eval(input);
    Logger.log('Resultado: ' + result);
  } catch(e) {
    Logger.log('Error: ' + e.message);
  }
}
```

Dado que la entrada directa del usuario no es factible de la misma manera que un REPL tradicional en el entorno de Apps Script, puedes modificar la variable `input` manualmente y ejecutar `myREPL()` para probar expresiones.

2. **Ejecución de código de muestra**:

Supongamos que deseas evaluar `2+2`. Modificarías la función `myREPL` de la siguiente manera:

```javascript
function myREPL() {
  var input = '2+2'; // Introduce manualmente tu expresión aquí
  // El resto permanece igual...
}
```

Después de ejecutar `myREPL()`, verifica los Registros (Ver > Registros) para ver la salida, que debería leer algo así:

```
[20-xx-xxxx xx:xx:xx:xxx] Ingresa tu expresión:
[20-xx-xxxx xx:xx:xx:xxx] Resultado: 4
```

3. **Depurando con Logger**:

Para una depuración más compleja, intercala `Logger.log(variable);` dentro de tu código para imprimir los estados de las variables, ayudándote a entender el flujo y los estados intermedios de tus scripts.

## Análisis profundo
El concepto de un REPL está profundamente arraigado en la historia de la computación, derivado de los sistemas de tiempo compartido de la década de 1960 que permitían sesiones interactivas. Lenguajes como Lisp prosperaron en este entorno, ya que el REPL era crítico para su proceso de desarrollo iterativo. En contraste, Google Apps Script, que surgió mucho más tarde, está diseñado principalmente para la web, centrándose en automatizar tareas dentro del conjunto de Google más que en la programación iterativa basada en consola.

Google Apps Script no admite tradicionalmente sesiones de codificación interactivas en tiempo real de manera predeterminada debido a su naturaleza basada en la nube y su enfoque en la implementación de aplicaciones web. Su modelo de ejecución se centra en funciones desencadenadas por eventos web, disparadores basados en tiempo o invocación manual dentro del entorno, en lugar de bucles de retroalimentación instantánea proporcionados por un REPL.

Mientras que el REPL y el depurador improvisados dentro del Editor de Apps Script ofrecen cierto nivel de interactividad, no replican completamente la retroalimentación inmediata y la eficiencia de los REPL tradicionales encontrados en muchos lenguajes de programación. Los desarrolladores que buscan una experiencia REPL más auténtica con las tecnologías de Google podrían explorar entornos JavaScript externos o Node.js con las API de Google. Estos pueden proporcionar una sesión de codificación más receptiva e interactiva, aunque requiriendo más configuración y posiblemente saliendo del entorno directo de Apps Script.
