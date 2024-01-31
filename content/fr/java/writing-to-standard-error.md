---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
En Java, écrire dans `stderr` signifie sortir des messages d'erreur. Les programmeurs l'utilisent pour séparer les erreurs des sorties normales, facilitant le débogage et le logging.

## How to:
```java
public class StdErrExample {
    public static void main(String[] args) {
        System.out.println("Sortie standard (stdout)");
        System.err.println("Erreur standard (stderr)");
    }
}
```
Sortie :
```
Sortie standard (stdout)
Erreur standard (stderr)
```

## Deep Dive
Historiquement, `stderr` a été conçu pour que les messages d'erreur puissent être traités séparément de la sortie standard (`stdout`). Ses alternatives incluent l'écriture de journaux dans des fichiers ou d'autres flux personnalisés. Dans Java, `System.err` est un `PrintStream` préconfiguré, similaire à `System.out`, mais toujours destiné aux erreurs.

## See Also
- [JavaDoc PrintStream](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/PrintStream.html)
- [Oracle's guide on I/O Streams](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)
- [StackOverflow discussion on stdout vs stderr](https://stackoverflow.com/questions/3385201/confused-about-stdin-stdout-and-stderr)
