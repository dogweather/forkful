---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "C: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why: La importancia de convertir una cadena de caracteres a minúsculas

Convertir una cadena de caracteres a minúsculas es una tarea común en la programación, ya que puede ser necesaria para comparar o manipular datos de forma precisa. Al convertir una cadena a minúsculas, se garantiza que los caracteres sean iguales y se evitan problemas de compatibilidad o errores en la ejecución del código.

## How To: Cómo convertir una cadena a minúsculas en C

Para convertir una cadena de caracteres a minúsculas en C, es necesario utilizar la función `tolower()` incluida en la librería `ctype.h`. Esta función toma como parámetro un caracter y devuelve su equivalente en minúscula. Para aplicarla a una cadena completa, se debe recorrer cada uno de los caracteres y reemplazarlos con su versión en minúscula. A continuación, se presenta un ejemplo de código:

```
#include <stdio.h>
#include <ctype.h>

int main() {
    // Definir una cadena de prueba
    char cadena[] = "PROGRAMANDO EN C";
    // Recorrer cada caracter de la cadena y reemplazarlo con su versión en minúscula
    for (int i = 0; i < strlen(cadena); i++) {
        cadena[i] = tolower(cadena[i]);
    }
    // Imprimir la cadena en minúsculas
    printf("%s", cadena);
    return 0;
}

```

El resultado de este código sería: `programando en c`.

## Deep Dive: Un análisis más profundo de la conversión de cadenas a minúsculas en C

La función `tolower()` utiliza la tabla de caracteres ASCII para determinar el equivalente en minúscula de cada caracter. Esto significa que solo se pueden convertir caracteres alfabéticos, por lo que símbolos o números no serán afectados. También es importante tener en cuenta que la función no afecta a la cadena original, sino que crea una copia en minúsculas, por lo que es necesario asignarla a una nueva variable o asignarla nuevamente a la cadena original.

Además, al utilizar la función `tolower()`, se debe tener en cuenta la localización y el idioma en el que se está trabajando, ya que pueden haber diferencias en la tabla de caracteres ASCII. Por ejemplo, en español la Ñ es considerada una letra y su equivalente en minúscula es la ñ, mientras que en inglés no existe la Ñ y se mantiene igual en mayúscula y minúscula.

## See Also: Enlaces útiles para seguir aprendiendo

- Documentación de la función `tolower()`: https://www.cplusplus.com/reference/cctype/tolower/
- Tutorial de C en español: https://www.tutorialesprogramacionya.com/cya/detalleconcepto.php?punto=29&codigo=29&inicio=C
- Preguntas frecuentes sobre cadenas de caracteres en C: https://www.cplusplus.com/faq/sequences/strings/