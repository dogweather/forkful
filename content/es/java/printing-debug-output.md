---
title:    "Java: Impresión de salida de depuración"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué
Debugging es una parte esencial de la programación y puede ser una tarea complicada. Una forma de facilitar esta tarea es escribiendo mensajes de salida debug durante la ejecución del código. Estos mensajes pueden ayudar a identificar errores y entender el flujo del programa.

## Cómo hacerlo
Para imprimir mensajes de salida debug en Java, podemos usar la clase `System.out` y su método `println()`. Por ejemplo:

```Java
System.out.println("Mensaje debug aquí");
```

También podemos usar la clase `java.util.logging.Logger` para imprimir mensajes de salida en diferentes niveles de severidad. Por ejemplo:

```Java
import java.util.logging.Logger;

Logger logger = Logger.getLogger("MiClase");
logger.severe("Mensaje de error");
logger.info("Mensaje informativo");
```

El resultado de estas líneas de código sería el siguiente:

```
[SEVERE](MiClase) - Mensaje de error
[INFO](MiClase) - Mensaje informativo
```

Otra forma de imprimir mensajes de salida debug es usando la anotación `@Log` de la librería `lombok`. Esta anotación genera automáticamente un registro con el nombre de la clase y el método desde donde se llama a la anotación. Por ejemplo:

```Java
import lombok.extern.log4j.Log;

@Log
public class MiClase {
    public void metodoEjemplo() {
        log.debug("Mensaje de debug");
    }
}
```

La salida del código anterior sería:

```
[DEBUG](MiClase.metodoEjemplo) - Mensaje de debug
```

## Profundizando
Al imprimir mensajes de salida debug, es importante tener en cuenta que estos deben ser eliminados antes de enviar el código a producción, ya que pueden impactar en el rendimiento del programa. Además, es una buena práctica utilizar diferentes niveles de severidad para los mensajes, de manera que podamos filtrarlos y enfocarnos en los necesarios.

También es posible agregar información adicional a los mensajes de salida, como por ejemplo el valor de ciertas variables en un determinado punto del programa. Esto puede ser de gran ayuda para entender qué está sucediendo en el código y encontrar la fuente de un error.

## Ver también
- [Documentación de la clase `System.out`](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#out)
- [Documentación de la clase `java.util.logging.Logger`](https://docs.oracle.com/javase/8/docs/api/java/util/logging/Logger.html)
- [Lombok: Anotación `@Log`](https://projectlombok.org/api/lombok/extern/log4j/Log.html)