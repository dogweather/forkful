---
title:                "Uso de expresiones regulares"
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Las expresiones regulares, o regex, son secuencias de caracteres que forman un patrón de búsqueda. Los programadores las usan para encontrar, reemplazar o manipular texto de manera eficiente y precisa.

## Cómo usar:

```Java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String texto = "El número de contacto es 123-456-7890.";
        String regex = "\\d{3}-\\d{3}-\\d{4}";
        
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(texto);

        if (matcher.find()) {
            System.out.println("Número encontrado: " + matcher.group());
        }
    }
}
```

Salida de muestra:

```
Número encontrado: 123-456-7890
```

## Inmersión Profunda:

Históricamente, las expresiones regulares nacen en la década de 1950 y se popularizan en los '70 con el lenguaje Perl. Alternativas a regex incluyen el procesamiento manual de strings (menos eficiente) y bibliotecas de análisis (parsing) especializadas (más complejas). Internamente, Java utiliza DFA (autómatas finitos deterministas) y NFA (no deterministas) para implementar regex, lo cual afecta el rendimiento y capacidades de las expresiones.

## Ver También:

Para más información echa un vistazo a:

- [Documentación oficial de las clases Pattern y Matcher](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Java Regular Expressions Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [RegexOne](https://regexone.com/) – Aprende con ejercicios interactivos.