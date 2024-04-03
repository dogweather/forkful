---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:24.383275-07:00
description: "Come fare: Il supporto integrato di Java per le regex \xE8 principalmente\
  \ attraverso le classi `Pattern` e `Matcher` nel pacchetto `java.util.regex`. Ecco\
  \ un\u2026"
lastmod: '2024-03-13T22:44:43.298814-06:00'
model: gpt-4-0125-preview
summary: "Il supporto integrato di Java per le regex \xE8 principalmente attraverso\
  \ le classi `Pattern` e `Matcher` nel pacchetto `java.util.regex`."
title: Utilizzo delle espressioni regolari
weight: 11
---

## Come fare:
Il supporto integrato di Java per le regex è principalmente attraverso le classi `Pattern` e `Matcher` nel pacchetto `java.util.regex`. Ecco un semplice esempio per trovare e stampare tutte le occorrenze di una parola in una stringa, senza distinguere tra maiuscole e minuscole:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex è ottimo per l'analisi. Analizzare con regex è potente.";
        String parolaDaTrovare = "analisi";
        
        Pattern pattern = Pattern.compile(parolaDaTrovare, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Trovato '" + matcher.group() + "' in posizione " + matcher.start());
        }
    }
}
```

Output:
```
Trovato 'analisi' in posizione 16
Trovato 'Analizzare' in posizione 31
```

Per compiti come dividere le stringhe, puoi usare il metodo `split()` della classe `String` con una regex:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] linguaggi = text.split(",");
        
        for (String linguaggio : linguaggi) {
            System.out.println(linguaggio);
        }
    }
}
```

Output:
```
Java
Python
Ruby
JavaScript
```

Quando lavori con le regex in Java, potrebbero esserci casi in cui una libreria esterna può semplificare compiti complessi. Una delle librerie di terze parti più popolari per lavorare con le regex in Java è `Apache Commons Lang`. Offre utility come `StringUtils` che rendono alcuni compiti regex più semplici. Ecco come usarlo per contare le occorrenze di una sottostringa:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex rende più facile l'elaborazione del testo. Elaborare il testo con regex è efficiente.";
        String sottostringa = "elaborazione";
        
        int conteggio = StringUtils.countMatches(text, sottostringa);
        System.out.println("'" + sottostringa + "' appare " + conteggio + " volte.");
    }
}
```

Per usare Apache Commons Lang, devi includerlo nel tuo progetto. Se stai usando Maven, aggiungi questa dipendenza al tuo `pom.xml`:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- Controlla per la versione più recente -->
</dependency>
```

Output:
```
'elaborazione' appare 2 volte.
```
