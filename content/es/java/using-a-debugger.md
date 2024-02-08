---
title:                "Usando un depurador"
aliases:
- es/java/using-a-debugger.md
date:                  2024-01-26T03:49:31.983594-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/using-a-debugger.md"
---

{{< edit_this_page >}}

## Qué y Por Qué
Usar un depurador significa emplear una herramienta para probar y corregir errores en tu código. Los programadores lo hacen para entender el flujo de sus aplicaciones, identificar las fuentes de errores y verificar la lógica bajo ejecución.

## Cómo hacerlo:
Digamos que tienes un programa Java sencillo que está actuando de manera extraña, y no puedes descubrir por qué. Así es cómo iniciarías un depurador usando Eclipse, uno de los IDEs más populares para el desarrollo en Java:

Primero, asegúrate de haber establecido un punto de interrupción. Luego, haz clic derecho en el archivo, selecciona 'Depurar como', y haz clic en 'Aplicación Java'.

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // Establecer un punto de interrupción aquí
        int result = divide(a, b);
        System.out.println("El resultado es: " + result);
    }

    private static int divide(int numerador, int denominador) {
        // Otro buen lugar para un punto de interrupción
        return numerador / denominador;
    }
}
```

Haciendo esto, tu programa pausará en el punto de interrupción, y podrás inspeccionar variables, avanzar a través del código línea por línea y observar cómo se comporta tu programa.

Salida de muestra (en una consola de depuración):
```
Punto de interrupción alcanzado en la línea: int result = divide(a, b);
```

## Inmersión Profunda
El concepto de depuración ha existido desde los primeros días de la programación. La leyenda cuenta que el término "bug" (error) proviene de un insecto real encontrado dentro de una computadora por Grace Hopper, una pionera en el campo. Acelerando hasta hoy, contamos con IDEs sofisticados como IntelliJ IDEA, Eclipse y NetBeans que incluyen depuradores potentes.

Las alternativas a los depuradores de IDE incluyen el registro de sucesos, instrucciones de impresión (depurador del pobre), afirmaciones y herramientas de depuración independientes como jdb (Java Debugger) que forma parte del Java Development Kit (JDK).

Un depurador funciona permitiendo al programador pausar la ejecución (puntos de interrupción), avanzar a través del código, inspeccionar los valores de las variables, modificar esos valores al vuelo e incluso ejecutar bloque por bloque de código. El uso de un depurador se considera a menudo una técnica invaluable para desarrollar aplicaciones complejas donde rastrear la línea exacta de código que causa un problema puede ser comparado con encontrar una aguja en un pajar.

## Ver También
- La documentación oficial de Oracle sobre depuración: [Oracle Java SE Debugging](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- Guía de Eclipse sobre depuración: [Consejos de Depuración de Eclipse](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article4.php)
- VisualVM, una herramienta visual que integra varias herramientas de línea de comandos del JDK y capacidades de perfilado ligero: [VisualVM](https://visualvm.github.io/)
