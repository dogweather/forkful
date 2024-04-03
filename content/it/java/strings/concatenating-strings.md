---
date: 2024-01-20 17:35:04.416969-07:00
description: "La concatenazione di stringhe \xE8 l'unione di due o pi\xF9 stringhe\
  \ in un'unica. I programmatori la usano per manipolare testi, creare messaggi, o\
  \ combinare\u2026"
lastmod: '2024-03-13T22:44:43.301032-06:00'
model: gpt-4-1106-preview
summary: "La concatenazione di stringhe \xE8 l'unione di due o pi\xF9 stringhe in\
  \ un'unica."
title: Concatenazione di stringhe
weight: 3
---

## How to:
```Java
public class ConcatenazioneStringhe {
    public static void main(String[] args) {
        // Utilizzo dell'operatore +
        String saluto = "Ciao";
        String mondo = "mondo";
        String fraseCompleta = saluto + " " + mondo + "!";
        System.out.println(fraseCompleta); // Ciao mondo!

        // Utilizzo di concat()
        String esclamazione = "Che bella giornata";
        esclamazione = esclamazione.concat(", non trovi?");
        System.out.println(esclamazione); // Che bella giornata, non trovi?

        // Utilizzo di StringBuilder
        StringBuilder costruttore = new StringBuilder();
        costruttore.append(saluto).append(", ").append(mondo).append("!");
        System.out.println(costruttore.toString()); // Ciao, mondo!
    }
}
```

## Deep Dive
Storicamente in Java, nella fase iniziale, l'operatore `+` era il metodo principale per concatenare le stringhe. Non era il più efficiente, specialmente per concatenazioni multiple in cicli, dato che ogni concatenazione creava un nuovo oggetto `String`, risultando in un uso della memoria non ottimale.

Le alternative moderne includono `StringBuilder` e `StringBuffer`. `StringBuilder` è più veloce e viene usato per la concatenazione di stringhe in un contesto single-thread, mentre `StringBuffer` è thread-safe, ma leggermente più lento a causa del suo overhead di sincronizzazione.

Dettagli di implementazione: A partire da Java 5, l'operatore `+` è stato ottimizzato per usare `StringBuilder` internamente nei cicli, mitigando queste preoccupazioni di performance. Tuttavia, per l'elaborazione esplicita di stringhe grandi o molto frequenti, si raccomanda ancora di usare direttamente `StringBuilder` per una gestione più fine della memoria e della performance.

## See Also
- [Java String](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html) - Documentazione ufficiale.
- [StringBuilder vs StringBuffer](https://www.baeldung.com/java-string-builder-string-buffer) - Differenze e quando usarli.
