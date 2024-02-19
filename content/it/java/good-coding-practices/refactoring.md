---
aliases:
- /it/java/refactoring/
date: 2024-01-26 01:18:50.344627-07:00
description: "Il Refactoring \xE8 il processo di ristrutturazione del codice informatico\
  \ esistente\u2014cambiando il factoring\u2014senza alterarne il comportamento esterno.\
  \ I\u2026"
lastmod: 2024-02-18 23:08:55.773542
model: gpt-4-0125-preview
summary: "Il Refactoring \xE8 il processo di ristrutturazione del codice informatico\
  \ esistente\u2014cambiando il factoring\u2014senza alterarne il comportamento esterno.\
  \ I\u2026"
title: Rifattorizzazione
---

{{< edit_this_page >}}

## Cosa & Perché?
Il Refactoring è il processo di ristrutturazione del codice informatico esistente—cambiando il factoring—senza alterarne il comportamento esterno. I programmatori lo fanno per migliorare gli attributi non funzionali del software, migliorando la leggibilità, riducendo la complessità e rendendo il codice più mantenibile per future imprese.

## Come fare:
Prendiamo una semplice classe Java che chiede a gran voce un refactoring a causa della sua cattiva organizzazione e mancanza di chiarezza.

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // Altre operazioni...
    }
}
```

Dopo il refactoring, abbiamo:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // Altre operazioni...
}
```

Con il refactoring, abbiamo migliorato i nomi dei metodi e i parametri per la leggibilità e eliminato la necessità di un ramo condizionale all'interno di un singolo metodo. Ogni operazione ora dichiara chiaramente il suo scopo.

## Approfondimento:
Il refactoring ha le sue radici nella comunità Smalltalk, con la sua enfasi sulla leggibilità del codice e sul design orientato agli oggetti, ma ha realmente preso piede nel mondo Java alla fine degli anni '90 e all'inizio degli anni '00, in particolare dopo la pubblicazione del libro fondamentale di Martin Fowler, "Refactoring: Miglioramento del design del codice esistente."

Ci sono alternative al refactoring, come riscrivere il codice da zero. Tuttavia, il refactoring è spesso preferito perché comporta cambiamenti incrementali che non interrompono la funzionalità dell'applicazione.

I dettagli implementativi quando si fa refactoring in Java (o in qualsiasi linguaggio di programmazione) ruotano attorno alla comprensione degli smell del codice—indicatori di problemi più profondi nel codice. Alcuni smell includono metodi lunghi, classi grandi, codice duplicato e uso eccessivo di primitivi. Applicando pattern di refactoring come Estrai Metodo, Sposta Metodo o Sostituisci Temp con Query, gli sviluppatori possono affrontare sistematicamente questi smell garantendo che il codice rimanga funzionale in ogni momento.

Strumenti automatizzati, come il supporto al refactoring di IntelliJ IDEA, o plugin per Eclipse, possono facilitare il processo automatizzando i refactorings come rinominare variabili, metodi e classi, estrarre metodi o variabili, e spostare metodi o classi in diversi pacchetti o namespace.

## Vedi Anche:
- "Refactoring: Miglioramento del design del codice esistente" di Martin Fowler: https://martinfowler.com/books/refactoring.html
- Tecniche di refactoring su Refactoring.Guru: https://refactoring.guru/refactoring/techniques
- Refactoring automatizzato in Eclipse: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- Caratteristiche di refactoring di IntelliJ IDEA: https://www.jetbrains.com/idea/features/refactoring.html

Ciascuna di queste risorse fornisce o una base per comprendere i principi del refactoring o strumenti che possono essere sfruttati per mettere in pratica questi principi.
