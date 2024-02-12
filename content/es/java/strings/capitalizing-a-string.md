---
title:                "Capitalizando una cadena de texto"
aliases:
- /es/java/capitalizing-a-string.md
date:                  2024-02-03T19:05:31.079939-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando una cadena de texto"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Capitalizar una cadena implica modificar la primera letra de cada palabra en la cadena a mayúsculas, asegurando que el resto permanezcan en minúsculas. Esta tarea común de manipulación de cadenas es útil para formatear texto en aplicaciones, como preparar nombres de usuarios o títulos para ser mostrados de acuerdo a la convención o corrección gramatical.

## Cómo hacerlo:
La biblioteca estándar de Java no proporciona un método directo para capitalizar cadenas enteras de una vez, pero puedes lograr esto con una combinación de métodos integrados. Para necesidades más sofisticadas, bibliotecas de terceros como Apache Commons Lang ofrecen soluciones directas.

### Usando Métodos Incorporados de Java
Para capitalizar una cadena sin bibliotecas externas, puedes dividir la cadena en palabras, capitalizar la primera letra de cada una y luego reunirlas. Aquí tienes un enfoque simple:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // Salida: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean encontrado = false;
        for (int i = 0; i < chars.length; i++) {
            if (!encontrado && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                encontrado = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                encontrado = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

Este fragmento de código convierte toda la cadena a minúsculas, luego itera a través de cada carácter, capitalizando la primera letra de cada palabra. Considera los espacios, puntos y apóstrofos como separadores de palabras.

### Usando Apache Commons Lang

La biblioteca Apache Commons Lang ofrece una solución más elegante con el método `WordUtils.capitalizeFully()`, que maneja varios casos límite y delimitadores por ti:

```java
// Añadir dependencia: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // Salida: "Hello, World!"
    }
}
```

Para usar este método, necesitarás añadir la biblioteca Apache Commons Lang a tu proyecto. Este método de la biblioteca no solo capitaliza la primera letra de cada palabra sino que también convierte el resto de las letras en cada palabra a minúsculas, asegurando un patrón de capitalización consistente a lo largo de la cadena.
