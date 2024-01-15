---
title:                "Capitalizar una cadena"
html_title:           "C++: Capitalizar una cadena"
simple_title:         "Capitalizar una cadena"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Capitalizar una cadena de texto puede ser útil en muchas situaciones. Por ejemplo, si queremos mostrar un nombre completo en mayúsculas en una aplicación, o si queremos estandarizar el formato de entrada de datos en un programa.

## ¿Cómo hacerlo?

Para capitalizar una cadena de texto en C++, podemos usar la función `toupper()` en un bucle para recorrer cada carácter de la cadena y convertirlo a mayúscula. A continuación, se puede imprimir la cadena resultante. Aquí hay un ejemplo de código:

```C++
string cadena = "hola mundo";
for (int i = 0; i < cadena.length(); i++) {
    cadena[i] = toupper(cadena[i]);
}
cout << cadena << endl;
```

La salida de este código sería `HOLA MUNDO`. También podemos utilizar la función `transform()` de la biblioteca `algorithm` para capitalizar una cadena. Aquí hay un ejemplo:

```C++
string cadena = "hola mundo";
transform(cadena.begin(), cadena.end(), cadena.begin(), ::toupper);
cout << cadena << endl;
```

La salida sería la misma, `HOLA MUNDO`.

## Profundizando más

En C++, cada carácter tiene un valor numérico asignado, conocido como su código ASCII. El código ASCII para las letras minúsculas va del 97 al 122, mientras que el código ASCII para las mayúsculas va del 65 al 90. La función `toupper()` simplemente toma el valor numérico del carácter y resta 32 para convertirlo a su equivalente en mayúscula. También podemos usar la función `tolower()` para convertir un carácter a minúscula.

## Ver también

- [toupper() en cplusplus.com](http://www.cplusplus.com/reference/cctype/toupper/)
- [transform() en cplusplus.com](http://www.cplusplus.com/reference/algorithm/transform/)