---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:42.308597-07:00
description: "Obtener la fecha actual en Java es una operaci\xF3n fundamental que\
  \ permite a los programadores manipular objetos de fecha para operaciones tales\
  \ como\u2026"
lastmod: '2024-03-13T22:44:58.947779-06:00'
model: gpt-4-0125-preview
summary: "Obtener la fecha actual en Java es una operaci\xF3n fundamental que permite\
  \ a los programadores manipular objetos de fecha para operaciones tales como registro\
  \ (logging), c\xE1lculos de fechas y condiciones basadas en tiempo."
title: Obteniendo la fecha actual
weight: 29
---

## Cómo hacerlo:
Java ofrece múltiples maneras de obtener la fecha actual, usando tanto la antigua clase `java.util.Date` como el más nuevo paquete `java.time` (introducido en Java 8), que es más versátil e intuitivo.

### Usando `java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Ejemplo de salida: 2023-04-01
    }
}
```

### Usando `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // Ejemplo de salida: 2023-04-01T12:34:56.789
    }
}
```

### Usando `java.util.Date` (Legado)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // Ejemplo de salida: Sat Apr 01 12:34:56 BST 2023
    }
}
```

### Utilizando una Librería de Terceros: Joda-Time
Antes de Java 8, Joda-Time era el estándar de facto para fecha y hora en Java. Si estás trabajando en sistemas heredados o tienes preferencia por Joda-Time, aquí te mostramos cómo puedes usarlo para obtener la fecha actual:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Ejemplo de salida: 2023-04-01
    }
}
```
**Nota:** Aunque `java.util.Date` y Joda-Time todavía se utilizan, el paquete `java.time` es recomendado para proyectos nuevos debido a su inmutabilidad y API comprensiva para manejar fechas y horas.
