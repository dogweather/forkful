---
title:                "Registro de Actividades"
date:                  2024-01-26T01:07:57.752789-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Actividades"
programming_language: "Python"
category:             "Python"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/logging.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
La generación de registros (logging) es el proceso de grabar eventos de una aplicación mientras un programa se ejecuta, proporcionando un rastro que puede ser analizado después de la ejecución y monitoreado en tiempo real. Los programadores lo hacen porque ayuda a depurar problemas, monitorear el rendimiento y rastrear las acciones de los usuarios por motivos de seguridad y análisis.

## Cómo hacerlo:
Python viene con un módulo incorporado para la generación de registros. Aquí tienes una configuración básica:
```Python
import logging

# Configuración básica del registro
logging.basicConfig(level=logging.INFO)

# Mensajes de registro
logging.debug('Este es un mensaje de depuración')
logging.info('Información sobre lo que su programa acaba de hacer')
logging.warning('Un mensaje de advertencia')
logging.error('Ha ocurrido un error')
logging.critical('¡El programa no puede recuperarse!')
```
Cuando ejecutes este código, verás la siguiente salida (ya que el nivel predeterminado es WARNING, los mensajes debug e info no se mostrarán):
```
WARNING:root:Un mensaje de advertencia
ERROR:root:Ha ocurrido un error
CRITICAL:root:¡El programa no puede recuperarse!
```
También puedes configurar el registro para escribir en un archivo en lugar de la consola:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Ahora tus registros se dirigirán al archivo 'app.log'.

## Estudio Detallado
La generación de registros existe desde los primeros días de la programación, siendo los registros del sistema una de las formas más antiguas de almacenamiento persistente fuera de los archivos reales que contienen datos. Dejando la historia a un lado, el concepto principal de la generación de registros permanece esencialmente sin cambios, aunque las herramientas han evolucionado.

El módulo `logging` de Python es bastante poderoso y flexible. Permite a los programadores establecer diferentes niveles de registro (DEBUG, INFO, WARNING, ERROR, CRITICAL) que pueden ayudar en la categorización y filtrado de registros. Tiene un sistema jerárquico de registradores, lo que significa que puedes tener relaciones de padres e hijos entre registradores y propagar mensajes a lo largo de la cadena.

Las alternativas incluyen bibliotecas de terceros como Loguru o structlog, que ofrecen características mejoradas y una interfaz más sencilla que el módulo de registro incorporado. Pueden proporcionar una salida más atractiva, una mejor serialización de datos estructurados y formas más intuitivas de lidiar con la configuración del registro.

En cuanto a la implementación, al configurar el registro es importante hacerlo una vez al inicio de la aplicación. Se recomienda configurarlo a nivel de módulo utilizando `logging.getLogger(__name__)` para seguir las mejores prácticas del registro de Python.

La generación de registros no debería afectar drásticamente al rendimiento de una aplicación en circunstancias normales. Sin embargo, se debe tener cuidado con lo que se registra: una generación de registros demasiado verbosa, especialmente en los niveles de DEBUG, puede ralentizar una aplicación y llenar rápidamente el espacio de almacenamiento de archivos de registro.

## Vea también
Para más información sobre el módulo de generación de registros de Python, consulta el libro de recetas oficial de registro de Python para obtener algunos grandes ejemplos y mejores prácticas: https://docs.python.org/3/howto/logging-cookbook.html

Para un vistazo más profundo a la generación de registros estructurada y cómo puede ayudar a hacer los registros más informativos y fáciles de analizar, Loguru está bien documentado: https://loguru.readthedocs.io

Además, considera echar un vistazo a la metodología de la aplicación 12 factores, específicamente la sección sobre registros para la visión moderna sobre el registro de aplicaciones: https://12factor.net/logs
