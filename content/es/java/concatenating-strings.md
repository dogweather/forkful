---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué y qué es?

La concatenación de cadenas es la operación de unir dos o más cadenas en una sola. Los programadores la usan para combinar texto de una manera flexible y dinámica.

## Cómo hacerlo:

Lo más sencillo es utilizar el operador `+` para concatenar cadenas. Aquí un ejemplo:

```java
public class Main {
    public static void main (String[] args)
    {
        String texto1 = "Hola";
        String texto2 = " Mundo!";
        String textoCompleto = texto1 + texto2;
        
        System.out.println(textoCompleto);
    }
}
```
 
Este programa imprimirá `Hola Mundo!`.

## Un poco más de fondo:

* **Historia**: En las primeras versiones de Java, la concatenación de cadenas era lenta y consumia mucha memoria. Con el tiempo, Java ha mejorado la eficiencia de la concatenación de cadenas.

* **Alternativas**: Otros enfoques para concatenar cadenas incluyen utilizar `StringBuilder` o `StringBuffer`. Estas clases son especialmente útiles para concatenar en un bucle o cuando trabajas con hilos.

* **Detalles de implementación**: Bajo el capó, cuando usas `+` para concatenar, Java usa en realidad `StringBuilder`. Se crea un nuevo objeto `StringBuilder`, se agregan las cadenas y finalmente se convierte a una cadena.

## Ver también:

* [Clase String en Java](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
* [Clase StringBuilder en Java](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/StringBuilder.html)