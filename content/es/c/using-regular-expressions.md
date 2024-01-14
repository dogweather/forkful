---
title:    "C: Utilizando expresiones regulares"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por qué utilizar expresiones regulares en programación

Las expresiones regulares son una herramienta muy útil para realizar búsquedas y manipulación de texto en programas de C. Permiten una manipulación más sofisticada y precisa de cadenas de texto, lo que puede ahorrar tiempo y esfuerzo al escribir código.

## Cómo utilizar expresiones regulares en C

Para utilizar expresiones regulares en C, necesitarás incluir el encabezado "regex.h" en tu programa. Luego, puedes utilizar la función "regcomp" para compilar tu expresión regular y la función "regexec" para ejecutarla. Aquí tienes un ejemplo de código:

```C
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main()
{
    char *texto = "Hola, mi nombre es Juan y mi número de teléfono es 555-123-4567.";
    char *patron = "([0-9]{3})-([0-9]{3})-([0-9]{4})";
    regex_t regex;
    int resultado;

    resultado = regcomp(&regex, patron, 0);
    if (resultado)
    {
        printf("No se pudo compilar la expresión regular.\n");
        exit(1);
    }

    resultado = regexec(&regex, texto, 0, NULL, 0);
    if (!resultado)
    {
        printf("Número de teléfono encontrado: %.*s\n", regex.endp[0] - regex.begp[0], regex.begp[0]);
    }
    else if (resultado == REG_NOMATCH)
    {
        printf("No se encontró un número de teléfono.\n");
    }
    else
    {
        printf("Error al ejecutar la expresión regular.\n");
        exit(1);
    }

    regfree(&regex);

    return 0;
}
```

Este ejemplo busca un número de teléfono en una cadena de texto utilizando una expresión regular. Si un número de teléfono es encontrado, se imprime en la consola. Si no se encuentra ningún número de teléfono, se imprime un mensaje de error. Ten en cuenta que debes tener cuidado con el patrón utilizado, ya que una expresión regular incorrecta puede causar errores o resultados inesperados.

## Un vistazo más profundo al uso de expresiones regulares

Además de la función "regexec", también hay otras funciones que pueden ser útiles al trabajar con expresiones regulares en C. Por ejemplo, "regerror" se utiliza para imprimir mensajes de error correspondientes a un código de error de expresión regular. También existe la función "regcomp_ex", que permite especificar opciones adicionales al compilar una expresión regular, como ignorar mayúsculas y minúsculas.

También es importante mencionar que hay diferentes tipos de expresiones regulares. Algunas de las más comunes incluyen "grep", "egrep" y "fgrep". Estos difieren ligeramente en su sintaxis y en los metacaracteres que se utilizan. Si ya estás familiarizado con los comandos de la consola, es posible que hayas utilizado estas expresiones regulares antes.

# Ver también

- Tutorial sobre expresiones regulares en C: https://www.rexegg.com/regex-c.html
- Documentación de la biblioteca regex.h de C: http://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
- Ejemplos de expresiones regulares en C: https://www.linuxtopia.org/online_books/programming_books/gnu_libc_guide/Part_15.html