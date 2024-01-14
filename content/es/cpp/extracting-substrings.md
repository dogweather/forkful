---
title:                "C++: Extrayendo subcadenas"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, hay ocasiones en las que necesitamos trabajar con una parte específica de una cadena de texto. Por ejemplo, puede que queramos extraer un nombre de usuario de una dirección de correo electrónico o separar una fecha en día, mes y año. Para resolver este tipo de problemas, podemos utilizar una técnica llamada "extracción de subcadenas" en C++. En esta entrada de blog, te enseñaremos cómo hacerlo.

## Cómo

Para extraer una subcadena de una cadena de texto en C++, utilizamos la función `substr()`. Esta función toma dos parámetros: el índice de inicio de la subcadena y el número de caracteres que deseamos extraer. Por ejemplo, si queremos extraer la subcadena "mundo" de la cadena "Hola mundo", utilizaríamos la función de la siguiente manera:

```C++
string cadena = "Hola mundo";
string subcadena = cadena.substr(5,5);

cout << subcadena; // Salida: mundo
```

También podemos especificar solo el índice de inicio y la función `substr()` extraerá automáticamente el resto de la cadena:

```C++
string cadena = "Hola mundo";
string subcadena = cadena.substr(5); // Extrae desde el índice 5 hasta el final de la cadena

cout << subcadena; // Salida: mundo
```

También podemos utilizar la función con variables, lo que hace que sea más dinámico y nos permite extraer diferentes subcadenas en base a diferentes condiciones. En este ejemplo, utilizamos la longitud de la palabra "mundo" para extraer la subcadena a partir de ese punto:

```C++
string cadena = "Hola mundo";
int pos = cadena.find("mundo"); // Encuentra el índice de la primera letra de la palabra "mundo"
string subcadena = cadena.substr(pos, "mundo".length()); // Extrae desde ese índice hasta la longitud de la palabra "mundo"

cout << subcadena; // Salida: mundo
```

## Deep Dive

Ahora que sabes cómo utilizar la función `substr()` en C++, es importante entender cómo funcionan los índices en una cadena de texto. En C++, los índices comienzan en 0, lo que significa que la primera letra de una cadena tiene el índice 0, la segunda letra tiene el índice 1 y así sucesivamente. Es importante tener en cuenta esto al utilizar la función `substr()` ya que si especificamos un índice fuera del rango de la cadena, recibiremos un error.

Además, si la subcadena que queremos extraer es más larga que la cadena original, C++ simplemente extraerá los caracteres hasta el final de la cadena sin mostrar ningún error. Por ejemplo, si intentamos extraer una subcadena de 10 caracteres de la cadena "Hola mundo", que solo tiene un total de 9 caracteres, obtendremos la cadena completa como resultado.

## Ver también

Si quieres aprender más sobre la extracción de subcadenas en C++, te recomendamos revisar la documentación oficial de C++ sobre la función `substr()`: https://www.cplusplus.com/reference/string/string/substr/

También puedes encontrar más información y ejemplos sobre esta función en otros blogs y foros de programación. ¡Sigue practicando y verás cómo esta técnica puede ser útil en tus proyectos de programación!