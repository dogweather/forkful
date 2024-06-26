---
date: 2024-01-26 01:08:31.659203-07:00
description: "C\xF3mo hacerlo: En Swift, puedes escribir registros en la consola con\
  \ instrucciones `print` o con la API `os.log`, m\xE1s flexible, que se conecta con\
  \ el\u2026"
lastmod: '2024-03-13T22:44:59.422875-06:00'
model: gpt-4-1106-preview
summary: "En Swift, puedes escribir registros en la consola con instrucciones `print`\
  \ o con la API `os.log`, m\xE1s flexible, que se conecta con el Sistema de Registro\
  \ Unificado en las plataformas de Apple."
title: Registro de Actividades
weight: 17
---

## Cómo hacerlo:
En Swift, puedes escribir registros en la consola con instrucciones `print` o con la API `os.log`, más flexible, que se conecta con el Sistema de Registro Unificado en las plataformas de Apple.

```Swift
import os.log

let logger = OSLog(subsystem: "com.tuapp.dominio", category: "network")

func fetchData() {
    // Instrucción print simple
    print("Inicio de la obtención de datos")
    
    // Registrando evento de nivel informativo usando os.log
    os_log(.info, log: logger, "Obteniendo datos de la API.")
    
    do {
        let data = try performNetworkRequest()
        // Registrando evento de nivel de depuración
        os_log(.debug, log: logger, "Datos recibidos: %@", data.description)
    } catch {
        // Registrando evento de nivel de error
        os_log(.error, log: logger, "Fallo al obtener datos: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // Simular una petición de red
    return Data()
}
```

La salida de muestra en la consola podría parecerse a esto:

```
Inicio de la obtención de datos
Obteniendo datos de la API.
Datos recibidos: Algunos bytes de datos...
```

Para errores, podría ser:

```
Fallo al obtener datos: La conexión a Internet parece estar desconectada.
```

## Profundización
El registro en Swift adquiere nuevas capacidades y eficiencia con el Sistema de Registro Unificado introducido en iOS 10 y macOS Sierra. A diferencia de la instrucción `print` que va directamente a la consola, este sistema está basado en actividades y permite filtrar mensajes de registro en función de su importancia y si son construcciones de depuración o de lanzamiento.

El contexto histórico enmarca la evolución del registro en iOS y macOS desde simples instrucciones de impresión hacia herramientas completas que se integran con la aplicación Instruments y Consola, proporcionando maneras sofisticadas de analizar los registros.

Hay una gama de alternativas para el registro dentro de Swift, como las bibliotecas de terceros como CocoaLumberjack, que ofrece una capa de macros sobre el Sistema de Registro Unificado. Proporciona control mejorado sobre el formato del registro, gestión de archivos y opciones de rendimiento.

Por último, detalles de implementación; OSLog está diseñado no solo para ser eficiente sino también consciente de la privacidad, con la capacidad de ofuscar datos privados al registrar. Categoriza los registros en niveles de fallo, error, información y depuración, cada uno ofreciendo una granularidad diferente para la resolución de problemas.

## Ver también
- [Documentación del Sistema de Registro Unificado de Apple](https://developer.apple.com/documentation/os/logging)
- [Tutorial de registro de Ray Wenderlich](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [Repositorio GitHub de CocoaLumberjack](https://github.com/CocoaLumberjack/CocoaLumberjack)
