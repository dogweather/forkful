---
title:    "Java: Capitalizando una cadena"
keywords: ["Java"]
---

{{< edit_this_page >}}

##Por qué

¿Alguna vez has tenido que manipular una cadena de texto en tu programa Java y necesitabas que todas las primeras letras de cada palabra estuvieran en mayúscula? Si es así, entonces capitalizar una cadena de texto es una habilidad imprescindible que debes tener. En este post, te mostraré cómo capitalizar una cadena de texto de manera sencilla y eficiente en Java.

##Cómo hacerlo

Para capitalizar una cadena de texto en Java, podemos utilizar la clase `StringBuilder` y su método `append()` junto con el método `toUpperCase()` de la clase `Character`. Aquí hay un ejemplo de código:

```Java
//Definimos la cadena de texto original
String texto = "hola, ¿cómo estás?";

//Creamos un objeto StringBuilder
StringBuilder builder = new StringBuilder();

//Separamos la cadena original en un arreglo de palabras
String[] palabras = texto.split(" ");

//Iteramos sobre cada palabra del arreglo
for (String palabra : palabras) {
    //Convertimos la primera letra de cada palabra a mayúscula
    palabra = Character.toUpperCase(palabra.charAt(0)) + palabra.substring(1);
    //Agregamos la palabra al objeto StringBuilder
    builder.append(palabra).append(" ");
}

//Convertimos el objeto StringBuilder a una cadena de texto
String textoCapitalizado = builder.toString();

//Imprimimos la cadena capitalizada
System.out.println(textoCapitalizado);

//Output: Hola, ¿cómo estás?
```

##Profundizando más

Una forma más sencilla de capitalizar una cadena de texto en Java es utilizando el método `capitalize()` de la clase `StringUtils` de la librería Apache Commons. Este método convierte automáticamente la primera letra de cada palabra en mayúscula. Aquí hay un ejemplo de código:

```Java
import org.apache.commons.lang3.StringUtils;

//Definimos la cadena de texto original
String texto = "hola, ¿cómo estás?";

//Capitalizamos la cadena utilizando StringUtils
String textoCapitalizado = StringUtils.capitalize(texto);

//Imprimimos la cadena capitalizada
System.out.println(textoCapitalizado);

//Output: Hola, ¿cómo estás?
```

También debes tener en cuenta que estos métodos sólo capitalizan la primera letra de cada palabra, por lo que si tienes una cadena de texto en mayúsculas o con ciertas letras ya en mayúscula, no se verán afectadas al utilizar estos métodos.

##Véase también

- [Clase StringBuilder - Documentación de Java](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Clase Character - Documentación de Java](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html)
- [Clase StringUtils - Documentación de Apache Commons](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)