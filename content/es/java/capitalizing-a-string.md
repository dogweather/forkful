---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
simple_title:         "Capitalizando una cadena de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Capitalizar una cadena significa convertir la primera letra de cada palabra a mayúscula. Los programadores lo hacen para asegurar la consistencia en los datos, mejorar la legibilidad o cumplir con especificaciones estéticas y de formato.

## Cómo hacerlo:

Para capitalizar una cadena en Java, la clase `String` es tu amiga. Aquí tienes un ejemplo:

```java
public class Capitalizador {
    public static void main(String[] args) {
        String frase = "java es genial";
        String fraseCapitalizada = capitalizarCadena(frase);
        System.out.println(fraseCapitalizada);  // Salida: Java Es Genial
    }

    public static String capitalizarCadena(String cadena) {
        String[] palabras = cadena.split("\\s+");
        StringBuilder cadenaCapitalizada = new StringBuilder();

        for(String palabra : palabras) {
            String primeraLetra = palabra.substring(0, 1).toUpperCase();
            String restoLetras = palabra.substring(1);
            cadenaCapitalizada.append(primeraLetra).append(restoLetras).append(" ");
        }

        return cadenaCapitalizada.toString().trim();
    }
}
```

## Profundizando

Históricamente, capitalizar texto ha sido importante en la escritura y la tipografía, y eso se refleja en la programación. Es crucial para crear interfaces de usuario que se lean naturalmente. Además, ayuda a formatear datos de manera estándar, como nombres propios o títulos de libros.

Alternativas para capitalizar cadenas en Java incluyen el uso de bibliotecas de terceros como Apache Commons Lang con su método `capitalize`, o simplemente manejar casos más específicos a mano. Cada implementación tiene sus propias consideraciones, como la localización o el manejo de casos especiales (por ejemplo, apóstrofes en nombres como O'Malley).

La implementación mostrada arriba se basa en manipulación simple de cadenas. Divide la cadena original en palabras utilizando el método `split` y luego procesa cada palabra individual. Es importante tratar adecuadamente caracteres no alfabéticos y prestar atención a la eficiencia si trabajas con textos muy grandes.

## Ver También

- Documentación oficial de la clase `String` de Java: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html
- Apache Commons Lang `StringUtils`: https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html
- Patrones de diseño para manipulación de cadenas: https://refactoring.guru/design-patterns/java
