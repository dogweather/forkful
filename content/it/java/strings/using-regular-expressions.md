---
title:                "Utilizzo delle espressioni regolari"
aliases: - /it/java/using-regular-expressions.md
date:                  2024-02-03T19:17:24.383275-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo delle espressioni regolari"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Le espressioni regolari (regex) in Java ti permettono di definire schemi specifici per cercare, manipolare o validare stringhe nel tuo codice. I programmatori le usano per compiti come l'analisi di file di log, la validazione dell'input dell'utente o la ricerca di schemi specifici all'interno di testi, consentendo un elaborazione sofisticata delle stringhe con uno sforzo minimo.

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
