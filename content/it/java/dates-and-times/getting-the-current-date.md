---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:51.672818-07:00
description: "Ottenere la data corrente in Java \xE8 un'operazione fondamentale che\
  \ consente ai programmatori di manipolare gli oggetti data per operazioni quali\
  \ il\u2026"
lastmod: '2024-03-13T22:44:43.319269-06:00'
model: gpt-4-0125-preview
summary: "Ottenere la data corrente in Java \xE8 un'operazione fondamentale che consente\
  \ ai programmatori di manipolare gli oggetti data per operazioni quali il\u2026"
title: Ottenere la data corrente
---

{{< edit_this_page >}}

## Cosa e Perché?
Ottenere la data corrente in Java è un'operazione fondamentale che consente ai programmatori di manipolare gli oggetti data per operazioni quali il logging, il calcolo delle date e le condizioni basate sul tempo. È vitale nelle applicazioni in cui il tracciamento, la pianificazione e l'analisi dei dati temporali sono cruciali.

## Come fare:
Java offre diversi modi per ottenere la data corrente, utilizzando sia la vecchia classe `java.util.Date` che il più recente pacchetto `java.time` (introdotto in Java 8), che è più versatile e intuitivo.

### Utilizzando `java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Esempio di output: 2023-04-01
    }
}
```
### Utilizzando `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // Esempio di output: 2023-04-01T12:34:56.789
    }
}
```
### Utilizzando `java.util.Date` (Legacy)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // Esempio di output: Sab Apr 01 12:34:56 BST 2023
    }
}
```
### Utilizzando una Libreria di Terze Parti: Joda-Time
Prima di Java 8, Joda-Time era lo standard de-facto per la data e l'ora in Java. Se stai lavorando su sistemi legacy o hai una preferenza per Joda-Time, ecco come puoi usarlo per ottenere la data corrente:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Esempio di output: 2023-04-01
    }
}
```
**Nota:** Sebbene `java.util.Date` e Joda-Time siano ancora utilizzati, il pacchetto `java.time` è consigliato per i nuovi progetti grazie alla sua immutabilità e all'API completa per la gestione delle date e degli orari.
