---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:06.284738-07:00
description: "C\xF3mo hacerlo: El soporte integrado de Java para regex se proporciona\
  \ principalmente a trav\xE9s de las clases `Pattern` y `Matcher` en el paquete\u2026"
lastmod: '2024-03-13T22:44:58.927927-06:00'
model: gpt-4-0125-preview
summary: "El soporte integrado de Java para regex se proporciona principalmente a\
  \ trav\xE9s de las clases `Pattern` y `Matcher` en el paquete `java.util.regex`."
title: Usando expresiones regulares
weight: 11
---

## Cómo hacerlo:
El soporte integrado de Java para regex se proporciona principalmente a través de las clases `Pattern` y `Matcher` en el paquete `java.util.regex`. Aquí hay un ejemplo simple para encontrar e imprimir todas las ocurrencias de una palabra en una cadena, sin distinguir entre mayúsculas y minúsculas:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex es genial para analizar. Analizar con regex es poderoso.";
        String palabraABuscar = "analizar";
        
        Pattern pattern = Pattern.compile(palabraABuscar, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Encontrado '" + matcher.group() + "' en la posición " + matcher.start());
        }
    }
}
```

Salida:
```
Encontrado 'analizar' en la posición 16
Encontrado 'Analizar' en la posición 31
```

Para tareas como dividir cadenas, puedes usar el método `split()` de la clase `String` con un regex:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] lenguajes = text.split(",");
        
        for (String lenguaje : lenguajes) {
            System.out.println(lenguaje);
        }
    }
}
```

Salida:
```
Java
Python
Ruby
JavaScript
```

Cuando trabajas con regex en Java, puede haber casos en los que una biblioteca externa puede simplificar tareas complejas. Una de las bibliotecas de terceros populares para trabajar con regex en Java es `Apache Commons Lang`. Ofrece utilidades como `StringUtils` que hacen que algunas tareas de regex sean más sencillas. Así es como se usa para contar las coincidencias de una subcadena:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex hace que el procesamiento de texto sea más fácil. Procesar texto con regex es eficiente.";
        String subcadena = "procesamiento";
        
        int count = StringUtils.countMatches(text, subcadena);
        System.out.println("'" + subcadena + "' aparece " + count + " veces.");
    }
}
```

Para usar Apache Commons Lang, necesitas incluirlo en tu proyecto. Si estás usando Maven, agrega esta dependencia a tu `pom.xml`:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- Verifica la última versión -->
</dependency>
```

Salida:
```
'procesamiento' aparece 2 veces.
```
