---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Buscar y reemplazar texto consiste en encontrar una cadena de caracteres específica y sustituirla por otra nueva. Los programadores lo hacen para modificar, actualizar o corregir información en códigos, archivos y bases de datos, lo que aumenta la eficiencia y reduce el tiempo de trabajo.

## ¿Cómo hacerlo?
Vamos a implementar una función simple de búsqueda y reemplazo en C. Este es un caso donde reemplazaremos "Hola" por "Adios" en una cadena dada.
```C
#include <stdio.h>
#include <string.h>

void buscar_reemplazar(char *cadena, char *buscar, char *reemplazar) {
    char buffer[1024] = { 0 };
    char *insertar_punto = &buffer[0];
    const char *tmp = cadena;
    size_t buscar_len = strlen(buscar);
    size_t reemplazar_len = strlen(reemplazar);

    while (1) {
        const char *p = strstr(tmp, buscar);

        // Si no encuentra "buscar", copia el resto y sale.
        if (p == NULL) {
            strcpy(insertar_punto, tmp);
            break;
        }

        // Copia el contenido antes de "buscar" y luego copia en "reemplazar".
        memcpy(insertar_punto, tmp, p - tmp);
        insertar_punto += p - tmp;
        memcpy(insertar_punto, reemplazar, reemplazar_len);
        insertar_punto += reemplazar_len;

        // Ajusta el puntero de entrada de la cadena original después de "buscar".
        tmp = p + buscar_len;
    }

    strcpy(cadena, buffer);
}

int main() {
    char cadena[1024] = "Hola mundo, Hola todos!";
    printf("Antes de reemplazar: %s\n", cadena);
    buscar_reemplazar(cadena, "Hola", "Adios");
    printf("Después de reemplazar: %s\n", cadena);
    // Salida: Antes de reemplazar: Hola mundo, Hola todos!
    //         Después de reemplazar: Adios mundo, Adios todos!
    return 0;
}
```

## Inmersión profunda
El concepto de búsqueda y reemplazo ha sido esencial desde los primeros días del procesamiento digital de texto, y es una funcionalidad básica en la mayoría de editores de texto, procesadores de texto y lenguajes de programación.

Numerosas librerías brindan manejo avanzado de cadenas, incluyendo funciones de búsqueda y reemplazo, como `strstr` y `strcpy` de la biblioteca estándar que utilizamos en el ejemplo. Pero si necesitas mayor eficiencia o capacidades especializadas, es posible proporcionar tu propia implementación o buscar entre las múltiples librerías que existen.

En cuanto a los detalles de implementación, la función `strstr` busca la primera aparición de la subcadena, y `strcpy` y `memcpy` manipulan la cadena para realizar el reemplazo. Tenga en cuenta que debes manejar cuidadosamente las referencias de los punteros y considerar el tamaño de las cadenas para evitar errores o vulnerabilidades.

## Ver también
- "Funciones de la biblioteca string.h en C": puedes ver https://www.programiz.com/c-programming/library-function/string.h
- "Manejo de cadenas en C": vea más en https://www.geeksforgeeks.org/string-handling-c/ y https://www.learn-c.org/en/Strings
- "Librerías C para manejo de cadenas": como tidystring: https://github.com/adambarley/tidystring