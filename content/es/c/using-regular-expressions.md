---
title:                "Utilizando expresiones regulares"
html_title:           "C: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

¿Qué Y Por Qué?

Las expresiones regulares son patrones de búsqueda utilizados por programadores en lenguajes como C para buscar, validar y manipular texto. Los programadores las utilizan principalmente para facilitar tareas de búsqueda y filtrado de datos.

Cómo Hacerlo:

```
#include <regex.h>

int main() {
    char text[] = "¡Hola! ¡Bienvenido a mi artículo de programación!";
    // Definir patrón de búsqueda
    char pattern[] = "Bienvenido";
    // Crear una estructura para guardar los resultados
    regex_t regex;
    // Compilar el patrón de búsqueda
    int status = regcomp(&regex, pattern, 0);
    if (status == 0) {
        // Realizar la búsqueda en el texto
        status = regexec(&regex, text, 0, NULL, 0);
        if (status == 0) {
            // Patrón encontrado
            printf("¡Bienvenido encontrado!");
        }
        else if (status == REG_NOMATCH) {
            // Patrón no encontrado
            printf("¡Bienvenido no encontrado!");
        }
        else {
            // Error en la búsqueda
            printf("Error en la búsqueda");
        }
    }
    else {
        // Error en la compilación del patrón
        printf("Error en la compilación del patrón");
    }
    // Liberar memoria de la estructura
    regfree(&regex);
    return 0;
}
```

Resultado:

```
¡Bienvenido encontrado!
```

Profundizando Más:

Las expresiones regulares se remontan a los años 50 y se popularizaron en los 70 gracias a herramientas como el editor de texto vi y el comando grep en Unix. Aunque también existen otras formas de manipular texto, como el uso de funciones estándar de cadenas de texto en C, las expresiones regulares ofrecen una forma más poderosa y versátil de buscar y manipular patrones en el texto.

Enlaces Relacionados:

- Documentación oficial de expresiones regulares en C:
https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html

- Tutorial sobre expresiones regulares en C:
https://www.tutorialspoint.com/c_standard_library/regex_h.htm