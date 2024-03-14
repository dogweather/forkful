---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:51.421733-07:00
description: "In Java, gli array associativi, o mappe, consentono di memorizzare coppie\
  \ chiave-valore per una ricerca e una manipolazione efficienti dei dati. I\u2026"
lastmod: '2024-03-13T22:44:43.301997-06:00'
model: gpt-4-0125-preview
summary: "In Java, gli array associativi, o mappe, consentono di memorizzare coppie\
  \ chiave-valore per una ricerca e una manipolazione efficienti dei dati. I\u2026"
title: Utilizzo di array associativi
---

{{< edit_this_page >}}

## Cosa & Perché?

In Java, gli array associativi, o mappe, consentono di memorizzare coppie chiave-valore per una ricerca e una manipolazione efficienti dei dati. I programmatori li utilizzano per compiti come il conteggio delle occorrenze di elementi o il mappaggio degli utenti ai loro permessi perché offrono accesso e aggiornamenti rapidi.

## Come fare:

Java non ha array associativi incorporati come alcuni linguaggi, ma fornisce l'interfaccia `Map` e classi come `HashMap` e `TreeMap` per riempire quel ruolo. Ecco come utilizzare una `HashMap`:

```Java
import java.util.HashMap;
import java.util.Map;

public class ImparaLeMappe {
    public static void main(String[] args) {
        // Creare una HashMap
        Map<String, Integer> etàDegliAmici = new HashMap<>();
        
        // Aggiungere elementi
        etàDegliAmici.put("Alice", 24);
        etàDegliAmici.put("Bob", 30);
        etàDegliAmici.put("Charlie", 28);

        // Accedere agli elementi
        System.out.println("Età di Alice: " + etàDegliAmici.get("Alice"));
        
        // Gestire chiavi inesistenti
        System.out.println("Età di qualcuno non nella mappa: " + etàDegliAmici.getOrDefault("Dan", -1));

        // Iterare sugli elementi
        for (Map.Entry<String, Integer> voce : etàDegliAmici.entrySet()) {
            System.out.println(voce.getKey() + " ha " + voce.getValue() + " anni.");
        }
    }
}
```

Esempio di Output:

```
Età di Alice: 24
Età di qualcuno non nella mappa: -1
Alice ha 24 anni.
Bob ha 30 anni.
Charlie ha 28 anni.
```

`HashMap` è solo una delle implementazioni. Se le tue chiavi sono uniche e hai bisogno che siano ordinate, considera `TreeMap`. Per una mappa che mantiene l'ordine di inserimento, `LinkedHashMap` è l'opzione giusta.

## Approfondimento

Le mappe in Java fanno parte del Framework delle Collezioni, introdotto in JDK 1.2, ma hanno visto miglioramenti significativi nel corso degli anni, inclusa l'introduzione del metodo `forEach` in Java 8 per un'iterazione più semplice sulle voci. La scelta dell'implementazione della mappa (`HashMap`, `LinkedHashMap`, `TreeMap`) dovrebbe essere dettata dalle tue esigenze specifiche in termini di ordinamento e prestazioni. Ad esempio, `HashMap` offre prestazioni di tempo O(1) per le operazioni di base (get e put), assumendo che la funzione hash disperda gli elementi adeguatamente tra i bucket. Tuttavia, se hai bisogno di ordinamento basato sull'ordinamento naturale o su comparatori personalizzati, `TreeMap` è la soluzione ideale, fornendo un tempo O(log n) per l'inserimento e la ricerca.

Prima dell'introduzione di `Map`, gli array associativi erano solitamente implementati con due array paralleli (uno per le chiavi, uno per i valori) o strutture dati personalizzate con minore efficienza. Le alternative attuali a `Map` e alle sue implementazioni potrebbero includere librerie di terze parti che offrono mappe specializzate, come mappe bidirezionali (BiMap nella libreria Guava di Google) per i casi in cui è necessario trovare una chiave in base al suo valore in modo efficiente. Tuttavia, per la maggior parte dei casi d'uso in Java, le mappe della libreria standard sono robuste e sufficientemente flessibili per gestire il compito.
