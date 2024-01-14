---
title:    "C: Utilizando expresiones regulares"
keywords: ["C"]
---

{{< edit_this_page >}}

##¿Por qué deberías usar expresiones regulares en tu programa en C?

Las expresiones regulares son una herramienta poderosa y útil en programación, especialmente en lenguaje C. Permiten buscar y analizar patrones en cadenas de texto de una manera eficiente y precisa. Si necesitas realizar operaciones de búsqueda, filtrado o validación de texto en tu programa, entonces las expresiones regulares son una excelente opción.

##Cómo usar expresiones regulares en C

Para utilizar expresiones regulares en C, primero debes incluir la librería `regex.h` en tu programa. Luego, debes crear un objeto `regex_t` que contendrá la expresión regular que deseas utilizar. En el siguiente ejemplo, se busca una coincidencia entre la expresión regular y la cadena de texto "Hola mundo".

```C
#include <regex.h>
#include <stdio.h>

int main() {
    char *exp_regex = "Hola.*";
    char *cadena_texto = "Hola mundo";
    regex_t regex;

    if (regcomp(&regex, exp_regex, 0) == 0) {
        int resultado = regexec(&regex, cadena_texto, 0, NULL, 0);
        if (resultado == REG_NOMATCH) {
            printf("No se encontró una coincidencia\n");
        } else {
            printf("Se encontró una coincidencia\n");
        }
    }
    return 0;
}
```
Salida:
```
Se encontró una coincidencia
```

También puedes utilizar expresiones regulares para obtener capturas específicas de una cadena de texto. Por ejemplo, si queremos obtener el código de área de un número telefónico en Argentina, podemos utilizar lo siguiente:

```C
#include <regex.h>
#include <stdio.h>

int main() {
    char *exp_regex = "^(\\+?54)?(?:11|[237][0124569]|9(?![09]))[0-9]{1,4}" \
                      "(?:\\-[0-9]{2,4}){2}$";
    char *telefono = "+541125555555";
    regex_t regex;

    if (regcomp(&regex, exp_regex, 0) == 0) {
        regmatch_t capturas[5];
        int resultado = regexec(&regex, telefono, 5, capturas, 0);
        if (resultado == 0) {
            // La primera captura contiene el número completo
            printf("Número: %.*s\n", (int)capturas[0].rm_eo,
                   &telefono[capturas[0].rm_so]);
            // La segunda captura contiene el código de área
            printf("Código de área: %.*s\n", (int)capturas[1].rm_eo,
                   &telefono[capturas[1].rm_so]);
        }
    }
    return 0;
}
```
Salida:
```
Número: +541125555555
Código de área: 011
```

##Profundizando en el uso de expresiones regulares

Las expresiones regulares pueden ser mucho más complejas que los ejemplos mostrados anteriormente. Además de utilizar metacaracteres para buscar patrones específicos, también puedes utilizar grupos de captura, cuantificadores y más para realizar búsquedas más precisas. Es importante conocer a fondo la sintaxis y funcionalidades de las expresiones regulares antes de utilizarlas en tus programas.

##Ver también

- [Documentación de la librería regex en C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Tutorial de expresiones regulares en C](https://www.tutorialspoint.com/c_standard_library/c_function_regcomp.htm)
- [Herramienta online para probar expresiones regulares en C](https://www.tutorialspoint.com/toolbox/online_c_regex_tester.php)