---
title:                "Iniciando un nuevo proyecto"
aliases:
- es/java/starting-a-new-project.md
date:                  2024-01-20T18:03:55.102231-07:00
model:                 gpt-4-1106-preview
simple_title:         "Iniciando un nuevo proyecto"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (¿Qué y Por Qué?)
Empezar un nuevo proyecto en Java es configurar un entorno desde cero para codificar una aplicación fresca. Programadores lo hacen para innovar, resolver problemas, aprender o ganar dinero.

## How to: (Cómo hacerlo:)
Para iniciar un proyecto en Java, necesitas herramientas como JDK y un IDE. Aquí te muestro cómo hacerlo con el JDK actual y el IDE IntelliJ IDEA. Primero, asegúrate de tener instalado Java:

```java
java -version
```

Este comando te debe dar la versión actual. Si no es así, descarga e instala el [JDK](https://www.oracle.com/java/technologies/javase-jdk11-downloads.html).

Ahora, creemos un nuevo proyecto con IntelliJ IDEA:

1. Abre IntelliJ IDEA.
2. Selecciona `File > New > Project`.
3. En la ventana de New Project, selecciona Java del panel de opciones y asegúrate de que tu JDK está seleccionado.
4. Clic en `Next`, luego `Finish`, y dale un nombre al proyecto.

Aquí tienes un "Hola Mundo" básico:

```java
public class Main {
    public static void main(String[] args) {
        System.out.println("Hola, Mundo!");
    }
}
```

Ejecútalo. Deberías ver:

```
Hola, Mundo!
```

## Deep Dive (Inmersión Profunda)
Java ha estado aquí desde 1995. Su popularidad no es casualidad. Su "escribe una vez, corre en cualquier lugar" la hizo destacar. Antes, lenguajes como C++ necesitaban compilación especifica para cada sistema operativo. Hoy, otras herramientas como Maven o Gradle automatizan la construcción y gestión de proyectos. También hay alternativas al IDE como Eclipse o NetBeans. Para proyectos más complejos, puedes explorar el soporte de Java para microservicios, nubes e inteligencia artificial.

## See Also (Consulta También)
- [Documentation for Java SE](https://docs.oracle.com/javase/)
- [Oracle's Java Tutorials](https://docs.oracle.com/javase/tutorial/)
- [IntelliJ IDEA Documentation](https://www.jetbrains.com/idea/documentation/)
- [Maven](https://maven.apache.org/)
- [Gradle](https://gradle.org/)
