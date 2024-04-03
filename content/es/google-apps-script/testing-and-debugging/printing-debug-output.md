---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:41.238678-07:00
description: "Imprimir salida de depuraci\xF3n implica colocar estrat\xE9gicamente\
  \ declaraciones de registro en su c\xF3digo para mostrar los valores de las variables,\
  \ el flujo\u2026"
lastmod: '2024-03-13T22:44:58.534008-06:00'
model: gpt-4-0125-preview
summary: "Imprimir salida de depuraci\xF3n implica colocar estrat\xE9gicamente declaraciones\
  \ de registro en su c\xF3digo para mostrar los valores de las variables, el flujo\
  \ de ejecuci\xF3n o mensajes de error durante el tiempo de ejecuci\xF3n."
title: "Imprimiendo salida de depuraci\xF3n"
weight: 33
---

## Cómo hacerlo:
Google Apps Script proporciona la clase `Logger` para la depuración básica, y para necesidades más avanzadas, la clase `console` introducida en el entorno de ejecución V8.

**Usando Logger:**

La clase Logger te permite registrar mensajes de depuración, que puedes ver después de la ejecución en el Editor de Apps Script bajo `Ver > Registros`. Aquí hay un ejemplo simple:

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Hola, %s!", name);
}
```

Después de ejecutar `logSample()`, puedes ver el registro con "¡Hola, Wired Reader!" en el visor de registros.

**Usando console.log con el entorno de ejecución V8:**

Con el entorno de ejecución V8, `console.log` proporciona una sintaxis más familiar para los desarrolladores que vienen de otros lenguajes:

```javascript
function consoleSample() {
  var status = 'activo';
  var count = 150;
  console.log(`Estado actual: ${status}, Cuenta: ${count}`);
}
```

Después de la ejecución, accede al registro de Stackdriver en `Ver > Registro de Stackdriver` para ver la salida. Es más poderoso, soportando la interpolación de cadenas y la inspección de objetos, e integra con los registros de Google Cloud, ofreciendo registros persistentes y capacidades avanzadas de filtrado.

**Salida de muestra de console.log:**

```
Estado actual: activo, Cuenta: 150
```

## Análisis Profundo
Inicialmente, `Logger.log` era la herramienta primaria para la depuración en Google Apps Script, ofreciendo una forma simple y directa de imprimir salida para su inspección. Sin embargo, a medida que los scripts se vuelven más complejos e integrados con los servicios de Google Cloud Platform, la necesidad de una solución de registro más robusta se hizo evidente.

Entonces llegó el entorno de ejecución V8, trayendo `console.log` a la escena. Esto no solo alinea Google Apps Script con la sintaxis estándar de JavaScript, haciendo el lenguaje más accesible para los desarrolladores familiarizados con JavaScript, sino que también aprovecha la poderosa infraestructura de las capacidades de registro de Google Cloud. La introducción de `console.log` y su integración con Google Cloud Platform marca una evolución significativa en las capacidades de depuración dentro de Google Apps Script, proporcionando a los desarrolladores un enfoque más dinámico y escalable para monitorear y solucionar problemas en sus scripts.

Mientras que `Logger.log` es suficiente para necesidades básicas de depuración y proyectos pequeños, `console.log` con el entorno de ejecución V8 ofrece una solución más comprensiva y a prueba de futuro. Esto incluye la capacidad de retener registros más allá de la sesión de ejecución, buscar y filtrar registros dentro de la consola de Google Cloud, y la alineación general con las prácticas modernas de desarrollo de JavaScript. Sin embargo, los desarrolladores deberían evaluar sus necesidades frente a la complejidad y escala de sus proyectos al elegir entre estas opciones.
