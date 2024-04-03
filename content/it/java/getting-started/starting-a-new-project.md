---
date: 2024-01-20 18:03:56.599373-07:00
description: "Avviare un nuovo progetto significa creare un ambiente per sviluppare\
  \ il tuo software. Lo fai per partire con un'organizzazione chiara e permettere\
  \ una\u2026"
lastmod: '2024-03-13T22:44:43.309463-06:00'
model: gpt-4-1106-preview
summary: Avviare un nuovo progetto significa creare un ambiente per sviluppare il
  tuo software.
title: Avvio di un nuovo progetto
weight: 1
---

## How to: (Come fare)
Creiamo una semplice applicazione Java che saluta l'utente. 

```Java
// HelloWorld.java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Ciao, mondo!");
    }
}
```

Output:
```
Ciao, mondo!
```

## Deep Dive (Approfondimento)
L'avvio di un progetto Java non era così diretto fino alla release del JDK 11, che ha introdotto il lancio di file `.java` singoli, senza la necessità di compilare esplicitamente. Prima, dovevi compilare in bytecode con `javac` e poi eseguire la classe con `java`.

```shell
javac HelloWorld.java
java HelloWorld
```

Un'alternativa moderna è l'utilizzo di strumenti come Maven o Gradle che aiutano nella gestione delle dipendenze e nell'automazione del build. Si parte da un file `pom.xml` per Maven o `build.gradle` per Gradle che descrive il progetto.

Il file `pom.xml` di base per un progetto Maven:

```xml
<project xmlns="http://maven.apache.org/POM/4.0.0">
    <modelVersion>4.0.0</modelVersion>
    <groupId>it.esempio</groupId>
    <artifactId>saluta-mondo</artifactId>
    <version>1.0-SNAPSHOT</version>
</project>
```

Gradle utilizza una sintassi Groovy o Kotlin per farlo. Questo è un esempio Groovy `build.gradle`:
```groovy
apply plugin: 'java'

group 'it.esempio'
version '1.0-SNAPSHOT'

repositories {
    mavenCentral()
}

dependencies {
    testImplementation 'junit:junit:4.12'
}
```

Per avviare i progetti Java moderni, si raccomanda di utilizzare l'IDE (Integrated Development Environment) come IntelliJ IDEA o Eclipse che forniscono wizard per semplificare questi processi.

## See Also (Vedi anche)
- Oracle Java Documentation: [https://docs.oracle.com/en/java/](https://docs.oracle.com/en/java/)
- Maven Getting Started: [https://maven.apache.org/guides/getting-started/](https://maven.apache.org/guides/getting-started/)
- Gradle Documentation: [https://docs.gradle.org/](https://docs.gradle.org/)
- IntellJ IDEA Documentation: [https://www.jetbrains.com/idea/documentation/](https://www.jetbrains.com/idea/documentation/)
- Eclipse Foundation: [https://www.eclipse.org/](https://www.eclipse.org/)
