---
title:                "Registro de Actividades"
aliases:
- es/typescript/logging.md
date:                  2024-01-26T01:08:47.263501-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Actividades"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/logging.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El registro (logging) es el proceso de grabar eventos, errores y otra información significativa durante la ejecución de un programa en un medio externo, a menudo archivos o bases de datos. Los programadores utilizan registros para monitorear el comportamiento del software, depurar problemas y rastrear actividades del sistema para análisis de seguridad y rendimiento.

## Cómo hacerlo:

En TypeScript, puedes implementar fácilmente un registro básico utilizando métodos de la consola o integrar un registro más avanzado con librerías como `winston` o `pino`. Aquí hay un ejemplo básico usando `console.log` y otro más avanzado con `winston`.

```TypeScript
// Registro básico en consola
console.log('Info: Iniciando la aplicación...');
console.error('Error: No se pueden recuperar los datos.');

// Salida de ejemplo
// Info: Iniciando la aplicación...
// Error: No se pueden recuperar los datos.
```

Para un registro más robusto, configuramos `winston`:

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('¡Servidor iniciado!');
logger.warn('Advertencia de espacio en disco bajo.');
logger.error('Fallo al conectar con la base de datos.');

// Salida de ejemplo en combined.log
// 2023-01-20 14:42:07 info: ¡Servidor iniciado!
// 2023-01-20 14:42:09 warn: Advertencia de espacio en disco bajo.
// 2023-01-20 14:42:12 error: Fallo al conectar con la base de datos.
```

## Análisis En Profundidad:

El concepto de registro dentro del contexto de la informática se remonta a los inicios de la programación, donde el término en sí proviene del "logbook" o bitácora, un sistema de registro utilizada en navegación. Históricamente, los eventos del programa a menudo se registraban en impresiones físicas o salidas de terminal, especialmente durante la era de los mainframes.

Avanzando hasta hoy, hay una plétora de herramientas y bibliotecas a tu disposición que atienden a diversas necesidades de registro, desde archivos de texto simples hasta sistemas complejos de gestión de registros. Alternativas a `winston` incluyen `pino`, que presume de un alto rendimiento, y `Bunyan`, que se basa en JSON. Al trabajar con Node.js, las bibliotecas de registro a menudo proporcionan mecanismos de flujo para canalizar los registros a diferentes destinos, soporte para la rotación de registros y formateadores personalizables.

En cuanto a la implementación, los mensajes de registro normalmente contienen una marca de tiempo, un nivel de gravedad (como info, warn, error) y el mensaje real. Una buena práctica de registro recomienda categorizar adecuadamente los niveles de registro, evitar datos sensibles en los registros y considerar implicaciones de rendimiento en aplicaciones de alto tráfico.

## Vea También:

- [Winston - Un registrador para casi todo](https://www.npmjs.com/package/winston)
- [Pino - Registrador de Node.js con muy bajo overhead](https://www.npmjs.com/package/pino)
- [Mejores Prácticas de Registro en Node.js](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [La Aplicación de 12 Factores - Registros](https://12factor.net/logs)
