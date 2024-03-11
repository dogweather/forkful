---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:50.041102-07:00
description: "El registro (logging) en programaci\xF3n implica grabar eventos, errores\
  \ u ocurrencias notables durante el tiempo de ejecuci\xF3n. Los programadores lo\
  \ hacen\u2026"
lastmod: '2024-03-11T00:14:32.393712-06:00'
model: gpt-4-0125-preview
summary: "El registro (logging) en programaci\xF3n implica grabar eventos, errores\
  \ u ocurrencias notables durante el tiempo de ejecuci\xF3n. Los programadores lo\
  \ hacen\u2026"
title: "Registro de Informaci\xF3n"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

El registro (logging) en programación implica grabar eventos, errores u ocurrencias notables durante el tiempo de ejecución. Los programadores lo hacen para depurar problemas, monitorear el rendimiento y mantener un registro de los datos operativos, lo que es fundamental para mantener y comprender el comportamiento del software en producción.

## Cómo hacerlo:

En Google Apps Script, el registro se puede realizar utilizando varios métodos, como la clase `Logger` y `console.log()`. La clase Logger es la forma tradicional, adecuada para la depuración simple y fines de desarrollo. Con las actualizaciones recientes, `console.log()` ofrece más flexibilidad e integración con Stackdriver Logging, proporcionando una solución más robusta para monitorear tus Apps Scripts en Google Cloud Platform.

**Usando Logger:**

```javascript
function logSample() {
  Logger.log('Este es un mensaje de registro simple');
  
  var value = 5;
  Logger.log('El valor es: %s', value); // Formato de cadena
}

// Para ver el registro:
// 1. Ejecuta la función logSample.
// 2. Ver -> Registros
```

**Salida del Logger de muestra:**

```
[22-04-20 10:00:00:000 PDT] Este es un mensaje de registro simple
[22-04-20 10:00:00:001 PDT] El valor es: 5
```

**Usando console.log():**

```javascript
function consoleLogSample() {
  console.log('Este mensaje va a Stackdriver Logging');
  const obj = {name: 'Jane', role: 'Desarrollador'};
  console.info('Registrando un objeto:', obj);
}

// Los registros se pueden ver en la consola de Google Cloud Platform (GCP) bajo Stackdriver Logging
```

**Salida de muestra de console.log():**

```
Este mensaje va a Stackdriver Logging
Registrando un objeto: {name: "Jane", role: "Desarrollador"}
```

Al pasar a `console.log()` para aplicaciones complejas, los desarrolladores pueden analizar y parsear registros de manera eficiente usando los poderosos filtros y herramientas proporcionados por GCP, lo cual no es tan directo con la clase Logger tradicional.

## Análisis profundo:

El registro en Google Apps Script ha evolucionado significativamente. Inicialmente, la clase `Logger` era el método principal para que los desarrolladores depuraran sus scripts. Es simple y suficiente para scripts básicos, pero carece de las capacidades necesarias para aplicaciones modernas en la nube, como buscar registros o analizar tendencias de registros a lo largo del tiempo.

La introducción de `console.log()` cerró esta brecha al integrar el registro de Google Apps Script con Stackdriver Logging de Google Cloud (ahora llamado Suite de Operaciones), proporcionando una plataforma centralizada para el registro, monitoreo y depuración de aplicaciones. Esto no solo permitió el registro a gran escala, sino que también abrió características avanzadas de gestión de registros como métricas basadas en registros, análisis de registros en tiempo real e integración con otros servicios de Google Cloud.

Mientras que `Logger` aún sirve para la depuración rápida y el registro en scripts más pequeños, la evolución hacia el uso de `console.log()` refleja un cambio más amplio en el desarrollo de aplicaciones escalables y nativas de la nube. Subraya el compromiso de Google de proporcionar a los desarrolladores herramientas que se adaptan a la complejidad y escala de las aplicaciones de hoy. Sin embargo, los recién llegados deben ser conscientes de la curva de aprendizaje un poco más empinada y de la necesidad de familiarizarse con los conceptos de Google Cloud Platform. A pesar de esto, el cambio es ventajoso para los desarrolladores que buscan aprovechar completamente las capacidades de la nube. Esta alineación con los servicios en la nube es parte de una tendencia más amplia en el desarrollo de software, que enfatiza la importancia de mecanismos de registro robustos y escalables en la era de la computación en la nube.
