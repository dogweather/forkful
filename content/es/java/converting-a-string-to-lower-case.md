---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La conversión de una cadena a minúsculas en Java significa transformar todas las letras mayúsculas de una cadena a su equivalente en minúsculas. Los programadores a menudo lo hacen para la normalización de datos o cuando la comparación de cadenas debe ser insensible a la capitalización.

## ¿Cómo hacerlo?

Aquí hay un simple ejemplo de cómo convertir una cadena a minúsculas en Java.

```Java 
String cadena = "HOLA, MUNDO!";
String cadenaEnMinusculas = cadena.toLowerCase();
System.out.println(cadenaEnMinusculas);
```

La salida será: "hola, mundo!".

## Inmersión Profunda

Históricamente, el método toLowerCase() se ha utilizado en Java desde su primera versión, lo que demuestra su importancia en la manipulación de cadenas. A lo largo de los años, este método ha permanecido relativamente constante, con sólo pequeñas optimizaciones y mejoras.

Durante la ejecución del método toLowerCase(), Java crea una nueva cadena (ya que las cadenas son inmutables en Java). Entonces, por cada carácter en la cadena original, si es una letra mayúscula, la reemplaza por su versión en minúscula utilizando el estándar Unicode.

Hay otras maneras de convertir una cadena a minúsculas sin usar toLowerCase(), aunque no son tan eficientes ni limpias. Por ejemplo, podríamos hacer un bucle a través de cada carácter en la cadena, comprobar si es una letra mayúscula utilizando el método Character.isUpperCase(), y si es así, convertirla a minúsculas con Character.toLowerCase().

```Java
String cadena = "HOLA, MUNDO!";
StringBuilder sb = new StringBuilder();

for (char c : cadena.toCharArray()) {
    if (Character.isUpperCase(c)) {
        sb.append(Character.toLowerCase(c));
    } else {
        sb.append(c);
    }
}
String cadenaEnMinusculas = sb.toString();
System.out.println(cadenaEnMinusculas);
```

## Ver También

Para obtener más información sobre la manipulación de cadenas en Java, consulte los siguientes enlaces:

- [La clase String en Java (Oracle Docs)](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [La clase Character en Java (Oracle Docs)](https://docs.oracle.com/javase/7/docs/api/java/lang/Character.html)
- [Java String toLowerCase() Method (Java T Point)](https://www.javatpoint.com/java-string-tolowercase)