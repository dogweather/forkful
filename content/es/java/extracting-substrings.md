---
title:    "Java: Extrayendo subcadenas"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# ¿Por qué extraer subcadenas en Java?

Extraer subcadenas es una habilidad fundamental en la programación en Java. Puede ser útil en muchas situaciones, como por ejemplo para manipular y analizar cadenas de texto.

## Cómo hacerlo

Para extraer una subcadena de una cadena existente en Java, podemos utilizar el método `substring()` de la clase `String`. Este método acepta dos parámetros: el índice de inicio y el índice de fin de la subcadena que deseamos extraer. A continuación, podemos ver un ejemplo de cómo utilizarlo:

```Java
String cadena = "Hola mundo";
String subcadena = cadena.substring(0, 4); // el índice de inicio es inclusivo, mientras que el índice de fin es exclusivo
System.out.println(subcadena); // esto imprimirá "Hola"
```

También podemos utilizar el método `substring()` para extraer una subcadena a partir de un índice específico hasta el final de la cadena:

```Java
String subcadena2 = cadena.substring(5); // esto extraerá la subcadena "mundo"
System.out.println(subcadena2); // esto imprimirá "mundo"
```

Existen otros métodos útiles en Java para extraer subcadenas, como `startsWith()` y `endsWith()`, que nos permiten verificar si una cadena comienza o termina con una determinada subcadena.

## Profundizando en la extracción de subcadenas

Cuando extraemos una subcadena en Java, en realidad lo que estamos haciendo es crear una nueva instancia de la clase `String`, que contiene los caracteres de la subcadena especificada. Esto significa que la subcadena extraída está completamente separada de la cadena original.

Además, es importante tener en cuenta que los índices de las cadenas en Java comienzan en 0. Por lo tanto, si queremos extraer la primera letra de una cadena, tendremos que utilizar el índice 0. Si queremos extraer la última letra de una cadena, tendremos que utilizar el índice de la longitud de la cadena menos uno.

# Ver también

- Documentación oficial de Java sobre el método `substring()`: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#substring(int,int)
- Artículo de tutorial de Java sobre la manipulación de cadenas: https://www.geeksforgeeks.org/java-string-class/
- Vídeo tutorial sobre cómo extraer subcadenas en Java: https://www.youtube.com/watch?v=JLc-hWsPTUY