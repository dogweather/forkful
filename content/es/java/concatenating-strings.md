---
title:                "Java: Concatenando cadenas"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Concatenar cadenas es una habilidad básica en Java que permite combinar diferentes cadenas de texto para crear una sola cadena. Esta técnica es útil cuando se trabaja con aplicaciones que requieren la creación de mensajes o impresión de información en la pantalla.

## Cómo hacerlo

La concatenación de cadenas se puede realizar de varias formas en Java. La forma más común es utilizando el operador `+` para unir cadenas. Por ejemplo:

```Java
String nombre = "Juan";
String apellido = "Pérez";
String nombreCompleto = nombre + " " + apellido;
System.out.println(nombreCompleto);
```

La salida de este código sería "Juan Pérez", ya que las cadenas se han unido utilizando el operador `+` entre ellas. También se pueden utilizar variables en la concatenación, lo que permite una mayor flexibilidad en la creación de cadenas.

Otra forma de concatenar cadenas es utilizando el método `concat()` de la clase `String`. Este método toma una cadena como argumento y la une a la cadena en la que se está llamando. Por ejemplo:

```Java
String nombreCompleto = nombre.concat(" ").concat(apellido);
System.out.println(nombreCompleto);
```

La salida sería la misma que en el ejemplo anterior. En este caso, se está utilizando el método `concat()` para unir las cadenas en vez del operador `+`.

## Profundizando

Es importante tener en cuenta que la concatenación de cadenas en Java puede ser costosa en términos de rendimiento y uso de memoria, especialmente cuando se trabaja con grandes cantidades de datos. En estos casos, es recomendable utilizar la clase `StringBuilder` en lugar de la concatenación tradicional.

La clase `StringBuilder` proporciona métodos eficientes para manipular cadenas sin crear objetos adicionales en memoria. Por ejemplo, se puede utilizar el método `append()` para agregar cadenas a un `StringBuilder` en lugar de utilizar el operador `+` o el método `concat()`. Luego, se puede obtener la cadena final llamando al método `toString()` del `StringBuilder`.

```Java
StringBuilder sb = new StringBuilder();
sb.append(nombre).append(" ").append(apellido);
String nombreCompleto = sb.toString();
System.out.println(nombreCompleto);
```

Esta es una forma más eficiente de concatenar cadenas cuando se trabaja con grandes cantidades de datos.

## Ver también

- [Documentación de Oracle sobre la clase `String`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Documentación de Oracle sobre la clase `StringBuilder`](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Ejemplos de concatenación de cadenas en Java](https://www.geeksforgeeks.org/stringconcat-methods-in-java-with-examples/)
- [Artículo sobre cómo mejorar el rendimiento al concatenar cadenas en Java](https://dzone.com/articles/efficient-string-concatenation-in-java)