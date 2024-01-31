---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Escribir en 'standard error' (stderr) significa mandar un mensaje a un flujo especial, usado para errores o diagnósticos. Lo hacemos para separar la info de errores de la salida regular ('standard output' o stdout), facilitando así el manejo de los errores y el debugging.

## Cómo hacerlo:
Aquí, dos ejemplos de cómo escribir en stderr en Java:

```Java
public class StderrExample {
    public static void main(String[] args) {
        System.err.println("Este es un error");
    }
}
```

Salida esperada:

```
Este es un error
```

Y para escribir una excepción en stderr:

```Java
public class StderrException {
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Se produjo un error: " + e.getMessage());
            e.printStackTrace(System.err);
        }
    }
}
```

Salida esperada:

```
Se produjo un error: / by zero
java.lang.ArithmeticException: / by zero
    at StderrException.main(StderrException.java:5)
```

## Profundizando
Escribir en stderr viene de los sistemas Unix, donde se definen tres flujos estándar: stdin, stdout y stderr (0, 1, y 2, respectivamente). Alternativas incluyen el uso de frameworks de logging como Log4j, que ofrecen más control sobre la salida. En Java, `System.err` es un `PrintStream` y utilizar sus métodos escribe directamente en stderr sin buffers, lo que significa que los mensajes de error se muestran inmediatamente.

## Ver También
- Documentación de la clase `System` de Java: [System JavaDocs](https://docs.oracle.com/javase/10/docs/api/java/lang/System.html)
- Tutorial de Oracle sobre excepciones: [Oracle Exceptions Tutorial](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Apache Log4j: [Apache Log4j 2](https://logging.apache.org/log4j/2.x/)
