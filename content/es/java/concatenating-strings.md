---
title:                "Concatenando cadenas"
html_title:           "Java: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/concatenating-strings.md"
---

{{< edit_this_page >}}

Qué es y por qué los programadores lo hacen

Concatenar cadenas de caracteres es un proceso común en programación que consiste en unir varias cadenas en una sola. Esto se hace con el fin de crear nuevas cadenas o imprimir mensajes personalizados en un programa. Los programadores utilizan la concatenación de cadenas para manipular datos de manera eficiente y lograr una mejor funcionalidad en sus programas.

Cómo hacerlo

Para concatenar cadenas de caracteres en Java, simplemente se utiliza el operador "+" entre dos o más cadenas. Por ejemplo:

```
String nombre = "Juan";
String saludo = "Hola ";
System.out.println(saludo + nombre);
```

La salida en pantalla sería: "Hola Juan". En este ejemplo, se unieron las cadenas "Hola" y "Juan" para formar una nueva cadena con el saludo y el nombre.

También se puede utilizar el método "concat()" de la clase String para concatenar cadenas. Por ejemplo:

```
String texto1 = "Este es un";
String texto2 = "ejemplo";
System.out.println(texto1.concat(" ").concat(texto2));
```

La salida en pantalla sería: "Este es un ejemplo". En este caso, se utilizó el método concat() para unir las cadenas con un espacio en blanco entre ellas.

Aunque la concatenación de cadenas se utiliza principalmente para unir cadenas, también se puede utilizar para convertir otros tipos de datos en cadenas. Por ejemplo:

```
int numero = 10;
System.out.println("El número es: " + numero);
```
La salida en pantalla sería: "El número es: 10". En este caso, el valor de la variable "numero" se convierte automáticamente en una cadena para poder ser concatenado.

Profundizando en el tema

La concatenación de cadenas de caracteres es una técnica muy común en la programación y se puede utilizar en diferentes lenguajes, no solo en Java. Es una forma eficiente de manipular datos y crear mensajes personalizados en programas.

Existen alternativas a la concatenación, como el uso del método "format()" de la clase String para formatear cadenas, o el uso de la clase StringBuilder para unir cadenas de manera más eficiente en términos de rendimiento. Sin embargo, la concatenación sigue siendo una forma sencilla y fácil de unir cadenas, especialmente para pequeñas cantidades de datos.

En cuanto a la implementación, Java utiliza una técnica llamada "StringBuilder concatenation" en la que se crea un objeto StringBuilder para almacenar las cadenas a concatenar y luego se convierte en una cadena final mediante el método toString(). Esto se hace detrás de escena y el programador no tiene que preocuparse por ello.

Para saber más sobre concatenar cadenas de caracteres en Java, puedes consultar la documentación oficial de Java o buscar tutoriales y ejemplos en línea.

Otras lecturas

- Documentación oficial de Java: https://docs.oracle.com/en/java/
- Tutorial de concatenación de cadenas en Java: https://www.w3schools.com/java/java_strings.asp
- Artículo sobre las diferencias entre concatenación y StringBuilder en Java: https://www.baeldung.com/java-string-concatenation