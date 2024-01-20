---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Convertir una cadena a minúsculas significa cambiar todas las letras mayúsculas en una cadena de texto a sus equivalentes en minúscula. Los programadores lo hacen para lograr la uniformidad en los datos de texto al comparar o procesar cadenas.

## ¿Cómo hacerlo?

En el lenguaje de programación C, puedes usar la función `tolower()` para convertir una cadena en minúsculas. Asegúrate de incluir la biblioteca `ctype.h`.

```C
#include <ctype.h>
#include <stdio.h>

void convertir_a_minusculas(char* str) {
    for(int i = 0; str[i]; i++){
        str[i] = tolower(str[i]);
    }
}

int main() {
    char str[] = "Hola Mundo!";
    convertir_a_minusculas(str);
    printf("%s", str);  
    return 0;
}
```

Cuando ejecutes este código, la salida será `hola mundo!`.

## Inmersión Profunda 

El lenguaje de programación C se remonta a los años 70, durante mucho tiempo antes de que se establecieran las convenciones modernas de codificación. La función `tolower()` ha sido una parte integral de C desde entonces, proporcionando una manera simple y eficaz de convertir cadenas a minúsculas.

Sin embargo, existen alternativas a `tolower()`. Por ejemplo, puedes implementar tu propia función para convertir cadenas a minúsculas, en especial si necesitas un comportamiento específico que `tolower()` no proporciona.

En cuanto a los detalles de implementación, `tolower()` trabaja en un solo carácter a la vez. Por eso, para convertir una cadena entera a minúsculas, necesitas recorrer cada carácter de la cadena y aplicar `tolower()` a cada uno de ellos.

## También Vea

Para más detalles y explicaciones, visita los siguientes enlaces:

1. Documentación oficial de `tolower()`: [aquí](https://en.cppreference.com/w/c/string/byte/tolower)
2. Análisis de la función `tolower()`: [aquí](https://www.programiz.com/c-programming/library-function/ctype.h/tolower)
3. La biblioteca `ctype.h` y sus funciones: [aquí](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)