---
title:                "Utilizando expresiones regulares"
html_title:           "C++: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Las expresiones regulares son una forma de buscar y manipular patrones de texto en un programa. Los programadores las usan para realizar tareas como validar entradas de usuario, extraer información de texto y reemplazar cadenas de caracteres.

## Cómo hacerlo:
Puedes usar expresiones regulares en C++ utilizando la biblioteca <regex> y el objeto std::regex. Por ejemplo, si quieres verificar si una cadena contiene solo números, puedes hacer lo siguiente:

```C++
std::string texto = "12345";
std::regex patron("[0-9]+");
if (std::regex_match(texto, patron)) {
    std::cout << "La cadena solo contiene números";
}
```

La salida de este código sería: "La cadena solo contiene números".

## Inmersión profunda:
Las expresiones regulares han existido desde la década de 1950 y se utilizan en muchos otros lenguajes de programación, además de C++. Alternativas para realizar operaciones de búsqueda y manipulación de patrones de texto incluyen el uso de loops y métodos de cadenas, pero las expresiones regulares ofrecen una forma más eficiente y precisa de hacerlo.

La implementación de las expresiones regulares en C++ se basa en el estándar POSIX y ofrece compatibilidad con diferentes sistemas operativos.

## Ver también:
- [Documentación de la biblioteca <regex> de C++]: https://stackoverflow.com/questions/25155838/how-to-make-regex-match-only-numeric-values
- [Expresiones regulares en otros lenguajes de programación]: https://www.regular-expressions.info/programming.html
- [Comodines y otros patrones de expresiones regulares]: https://www.rexegg.com/regex-quickstart.html