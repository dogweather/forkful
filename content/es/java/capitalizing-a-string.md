---
title:                "Capitalizando una cadena de texto"
html_title:           "Java: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

La capitalización de una cadena (string) implica cambiar la primera letra de la cadena a mayúsculas. Los programadores hacen esto para mejorar la legibilidad y la presentación de los datos.

## ¿Cómo hacerlo?
Con Java, puedes lograr esto usando varias técnicas. Aquí un ejemplo con el método `toUpperCase()` del objeto `Character`:

```Java
public class Principal {
    public static void main(String[] args) {
        String texto = "hola, mundo!";
        String resultado = texto.substring(0, 1).toUpperCase() + texto.substring(1);
        System.out.println(resultado);
    }
}
```
Salida de muestra:
```
Hola, mundo!
```

Aquí, tomamos la primera letra (índice 0), la convertimos en mayúsculas y luego añadimos el resto de la cadena.

## Inmersión profunda

1. **Contexto histórico**: Java ha incluido los métodos `toUpperCase()` y `toLowerCase()` desde su primera versión. Son métodos fundamentales de la clase `String`.

2. **Alternativas**: También puedes usar otras bibliotecas, como Apache Commons Lang, que proporciona el método `capitalize()` de la clase `StringUtils`.

```Java
import org.apache.commons.lang3.StringUtils;

public class AltPrincipal {
    public static void main(String[] args) { 
        String texto = "hola, mundo!";
        String resultado = StringUtils.capitalize(texto);
        System.out.println(resultado);
    }
}
```
3. **Detalle de la implementación**: El método `toUpperCase()` de Character usa las configuraciones de tu sistema local para determinar el carácter en mayúsculas correspondiente. Si necesitas evitar variaciones locales, puedes usar `toUpperCase(Locale.ENGLISH)`.

## Ver también

1. [Documentación oficial de Java para la clase String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
2. [Documentación de Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)