---
title:                "Eliminar caracteres coincidentes con un patrón."
html_title:           "C#: Eliminar caracteres coincidentes con un patrón."
simple_title:         "Eliminar caracteres coincidentes con un patrón."
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Borrar caracteres que coincidan con un patrón es un proceso en el que los programadores eliminan de un texto aquellos caracteres que se ajustan a un cierto conjunto de criterios. Esto puede ser útil para limpiar y organizar datos o para validar la entrada del usuario en una aplicación.

## Cómo hacerlo:
Aquí hay un ejemplo sencillo de cómo borrar todos los números de una cadena de texto utilizando el método de expresiones regulares en C#:

```
string texto = "abc123def456ghi";
string patron = @"\d+";
string resultado = Regex.Replace(texto, patron, "");
Console.WriteLine(resultado); // Salida: abcdefghi
```

También es posible borrar caracteres específicos utilizando el método `Remove()` de la clase `StringBuilder` en C#:

```
string texto = "Hola mundo!";
StringBuilder sb = new StringBuilder(texto);
sb.Remove(5, 1); // Elimina la letra 'm'
Console.WriteLine(sb.ToString()); // Salida: Holaundo!
```

## Profundizando:
Borrar caracteres que coincidan con un patrón es una forma eficiente de manipular grandes cantidades de datos en un texto. Esta técnica utiliza expresiones regulares, que son patrones utilizados para buscar o manipular texto. Alternativamente, también se puede utilizar el método `Replace()` de la clase `String` para reemplazar caracteres específicos en una cadena de texto.

En términos de implementación, los algoritmos utilizados para borrar caracteres que coincidan con un patrón pueden variar según el lenguaje de programación y la plataforma utilizados. En C#, la clase `Regex` proporciona métodos como `Replace()` y `Match()` que facilitan la manipulación de texto utilizando expresiones regulares.

## Ver también:
- [Documentación de Microsoft sobre el método Remove() en C#](https://docs.microsoft.com/es-es/dotnet/api/system.text.stringbuilder.remove)
- [Ejemplos de expresiones regulares en C#](https://www.dotnetperls.com/regex)
- [Guía de expresiones regulares en C#](https://www.c-sharpcorner.com/article/regex-in-C-Sharp/)