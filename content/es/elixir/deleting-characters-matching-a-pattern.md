---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

¿Qué es y por qué? 
Eliminar caracteres que coinciden con un patrón es una técnica común utilizada por los programadores para manipular cadenas de texto y limpiar datos. Se utiliza para eliminar caracteres no deseados que no encajan en un patrón especificado.

Cómo hacerlo:
Aquí hay un ejemplo sencillo de cómo eliminar caracteres que coinciden con un patrón en Elixir:

```Elixir
str = "Eli…..xir"
clean_str = Regex.replace(~r/[^A-Za-z]/, str, "")
IO.puts(clean_str)
# Output: Elixir 
```
En este ejemplo, especificamos un patrón utilizando expresiones regulares (`~r/[^A-Za-z]/`) que coincide con cualquier carácter que no sea una letra. Luego utilizamos la función `Regex.replace()` para reemplazar todos los caracteres que coinciden con este patrón con una cadena vacía. Finalmente, imprimimos el resultado utilizando `IO.puts()` y obtenemos la cadena "Elixir" sin los puntos suspensivos.

Profundizando:
Eliminar caracteres que coinciden con un patrón es una técnica que ha existido desde los primeros días de la programación de computadoras. Se utilizaba principalmente para limpiar datos y eliminar información innecesaria antes de realizar cálculos o análisis. En Elixir, también podemos utilizar la función `String.replace()` para lograr el mismo resultado, pero esta función solo funcionará con patrones más simples sin utilizar expresiones regulares.

Alternativas:
Además de utilizar la función `Regex.replace()`, también podemos utilizar la función `Regex.split()` para dividir una cadena en una lista basada en un patrón determinado y luego manipular la lista resultante. Otra alternativa es utilizar la función `String.delete_suffix()` o `String.delete_prefix()` para eliminar un sufijo o prefijo específico de una cadena.

Detalles de implementación:
La función `Regex.replace()` en Elixir utiliza la librería PCRE (Perl Compatible Regular Expressions, expresiones regulares compatibles con Perl) para procesar los patrones especificados. Esta librería está escrita en lenguaje C y es extremadamente rápida y eficiente. Al utilizar expresiones regulares en Elixir, también podemos aprovechar las características avanzadas de la sintaxis de expresiones regulares de PCRE para realizar operaciones más complejas y avanzadas en nuestras cadenas.

Ver también:
- Documentación de la función `Regex.replace()` en la página oficial de Elixir: https://hexdocs.pm/elixir/Regex.html#replace/3
- Ejemplos de uso de expresiones regulares en Elixir: https://elixir-examples.github.io/string/regex