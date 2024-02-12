---
title:                "Manejando errores"
aliases:
- es/c/handling-errors.md
date:                  2024-02-03T17:58:00.107013-07:00
model:                 gpt-4-0125-preview
simple_title:         "Manejando errores"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Manejar errores en C involucra detectar y responder a condiciones anómalas que surgen durante la ejecución del programa. Los programadores hacen esto para prevenir errores, caídas y comportamientos impredecibles, asegurando que el software funcione de manera fiable y eficiente bajo varios escenarios.

## Cómo hacerlo:

C no tiene soporte integrado para excepciones como algunos otros lenguajes. En cambio, depende de algunas estrategias convencionales para el manejo de errores, tales como retornar valores especiales de las funciones y configurar variables globales como `errno`.

**Retornando Valores Especiales**

Las funciones pueden indicar errores al retornar un valor específico que es improbable que sea un resultado válido. Aquí hay un ejemplo con enteros:

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // Caso de error
    } else {
        *result = 1.0 / number;
        return 0; // Éxito
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("Error: División por cero.\n");
    } else {
        printf("El inverso es: %f\n", result);
    }
    
    return 0;
}
```

**Salida:**
```
Error: División por cero.
```

**Comprobando `errno`**

Para funciones de la biblioteca, especialmente aquellas que interactúan con el sistema o el OS (como E/S de archivos), `errno` se configura cuando ocurre un error. Para usarlo, incluye `errno.h` y verifica `errno` después de una falla sospechosa:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("Error abriendo el archivo: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Salida:**
```
Error abriendo el archivo: No existe el archivo o el directorio
```

## Análisis Profundo

Históricamente, el diseño minimalista del lenguaje de programación C ha excluido un mecanismo integrado de manejo de excepciones, reflejando sus orígenes de programación de sistemas de bajo nivel donde el máximo rendimiento y el control cercano al hardware son críticos. En cambio, C adopta un enfoque de manejo de errores más manual que se ajusta a su filosofía de dar a los programadores tanto control como sea posible, incluso a costa de la conveniencia.

Aunque este enfoque se alinea bien con los objetivos de diseño de C, también puede llevar a código de verificación de errores verboso y al potencial de verificaciones de error perdidas, los cuales los lenguajes modernos abordan con mecanismos estructurados de manejo de excepciones. Por ejemplo, las excepciones en lenguajes como Java o C# permiten un procesamiento de errores centralizado, haciendo el código más limpio y la gestión de errores más directa. Sin embargo, las excepciones introducen su sobrecarga y complejidad, que podrían no ser ideales para la programación a nivel de sistema donde C brilla.

A pesar de su tosquedad, este manejo manual de errores en C ha informado el diseño de gestión de errores en muchos otros lenguajes, ofreciendo un modelo donde la explicitud de las condiciones de error puede llevar a un código más predecible y depurable. Para sistemas críticos, donde los fallos deben manejarse de manera elegante, el paradigma de manejo de errores de C—combinado con las mejores prácticas modernas como bibliotecas y convenciones de manejo de errores—asegura robustez y fiabilidad.
