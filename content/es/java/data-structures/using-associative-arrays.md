---
title:                "Uso de matrices asociativas"
aliases:
- /es/java/using-associative-arrays/
date:                  2024-01-30T19:12:04.156050-07:00
model:                 gpt-4-0125-preview
simple_title:         "Uso de matrices asociativas"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y por qué?

En Java, los arreglos asociativos, o mapas, te permiten almacenar pares de clave-valor para una búsqueda y manipulación de datos eficiente. Los programadores los usan para tareas como contar ocurrencias de elementos o mapear usuarios a sus permisos porque ofrecen acceso y actualizaciones rápidas.

## Cómo hacerlo:

Java no tiene arreglos asociativos incorporados como algunos lenguajes, pero proporciona la interfaz `Map` y clases como `HashMap` y `TreeMap` para cumplir ese rol. Aquí está cómo usar un `HashMap`:

```Java
import java.util.HashMap;
import java.util.Map;

public class AprendeMapas {
    public static void main(String[] args) {
        // Creando un HashMap
        Map<String, Integer> edadDeAmigos = new HashMap<>();
        
        // Agregando elementos
        edadDeAmigos.put("Alice", 24);
        edadDeAmigos.put("Bob", 30);
        edadDeAmigos.put("Charlie", 28);

        // Accediendo a elementos
        System.out.println("Edad de Alice: " + edadDeAmigos.get("Alice"));
        
        // Manejando claves inexistentes
        System.out.println("Edad de alguien no en el mapa: " + edadDeAmigos.getOrDefault("Dan", -1));

        // Iterando sobre elementos
        for (Map.Entry<String, Integer> entrada : edadDeAmigos.entrySet()) {
            System.out.println(entrada.getKey() + " tiene " + entrada.getValue() + " años.");
        }
    }
}
```

Salida de muestra:

```
Edad de Alice: 24
Edad de alguien no en el mapa: -1
Alice tiene 24 años.
Bob tiene 30 años.
Charlie tiene 28 años.
```

`HashMap` es solo una implementación. Si tus claves son únicas y necesitas que estén ordenadas, considera `TreeMap`. Para un mapa que retiene el orden de inserción, `LinkedHashMap` es tu amigo.

## Profundización

Los mapas en Java son parte del Marco de Colecciones, introducido en JDK 1.2, pero han visto mejoras significativas a lo largo de los años, incluyendo la introducción del método `forEach` en Java 8 para una iteración más fácil sobre las entradas. La elección de la implementación del mapa (`HashMap`, `LinkedHashMap`, `TreeMap`) debe ser dictada por tus necesidades específicas en términos de ordenamiento y rendimiento. Por ejemplo, `HashMap` ofrece un rendimiento en tiempo O(1) para las operaciones básicas (obtener y poner), asumiendo que la función hash dispersa los elementos adecuadamente entre los cubos. Sin embargo, si necesitas ordenamiento basado en el orden natural o comparadores personalizados, `TreeMap` es la opción a seguir, proporcionando un tiempo O(log n) para la inserción y búsqueda.

Antes de que se introdujera `Map`, los arreglos asociativos solían implementarse con dos arreglos paralelos (uno para claves, uno para valores) o estructuras de datos personalizadas con menos eficiencia. Las alternativas actuales a `Map` y sus implementaciones podrían incluir bibliotecas de terceros que ofrecen mapas especializados, como mapas bidireccionales (BiMap en la biblioteca de Google Guava) para casos en los que necesitas encontrar una clave por su valor de manera eficiente. Sin embargo, para la mayoría de los casos de uso en Java, los mapas de la biblioteca estándar son lo suficientemente robustos y flexibles para manejar la tarea.
