---
title:                "Registro de Actividades en Programación"
date:                  2024-01-26T00:58:40.900052-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Actividades en Programación"

category:             "Bash"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/logging.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El registro (logging) es la práctica de documentar eventos, errores y otra información significativa de los procesos en ejecución de un programa en un archivo o una corriente de salida. Los programadores lo hacen para rastrear el comportamiento de sus aplicaciones, depurar problemas y mantener un registro histórico de operaciones que puede ayudar con la resolución de problemas futuros.

## Cómo hacerlo:

En Bash, registrar puede ser tan simple como redirigir o anexar la salida a un archivo. Aquí hay un ejemplo básico:

```Bash
echo "Iniciando el script..." >> script.log
# Tus comandos del script aquí
echo "Script completado el $(date)" >> script.log
```

Para algo más avanzado, podrías incorporar syslog para un registro a nivel del sistema:

```Bash
logger "Mensaje personalizado de mi script"
```

`logger` envía un mensaje de registro al servicio syslog, que luego lo maneja de acuerdo con la configuración syslog del sistema.

Ejemplo de salida capturada en `script.log`:

```Bash
Iniciando el script...
Script completado el mar 23 mar 09:26:35 PDT 2021
```

## Estudio Profundo

Históricamente en sistemas tipo Unix, el registro ha sido facilitado por el servicio syslog, permitiendo que diferentes aplicaciones y partes del sistema registren mensajes de manera centralizada. Esto permite la implementación de un mecanismo de registro estandarizado en todo el sistema.

Cuando se trata de alternativas, algunos pueden considerar el uso de `syslog-ng` o `rsyslog` para funciones de registro más avanzadas, o escribir los registros en una base de datos de series temporales con fines analíticos. Para aplicaciones con niveles más altos de complejidad, utilizar una biblioteca o aplicación de registro dedicada como Log4j (en el ecosistema de Java) o Monolog (en PHP), que pueden proporcionar opciones de registro estructuradas y configurables, podría tener sentido incluso para un lenguaje de script como Bash.

La forma en que implementas el registro depende en gran medida de los requisitos de tu aplicación. Si solo necesitas una salida simple para rastrear el progreso del script, añadir mensajes a un archivo es fácil y conveniente. Sin embargo, para un registro más escalable y robusto, querrás integrarte con un sistema de registro que admita características como rotación de registros, niveles de registro y registro remoto.

## Ver También

- Las páginas `man` para las funciones `logger` y `syslog` siempre son tus aliados, intenta `man logger` o `man syslog`.
- Para una visión más profunda del registro del sistema, considera leer la documentación de `rsyslog` y `syslog-ng`.
- Para obtener más información sobre el contexto histórico y los principios detrás del registro en sistemas tipo Unix, el protocolo `Syslog` documentado en la RFC 5424 proporciona información comprensiva.
