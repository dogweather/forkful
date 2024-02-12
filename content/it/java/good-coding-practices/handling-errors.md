---
title:                "Gestione degli errori"
date:                  2024-01-26T00:53:36.234682-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Gestire gli errori significa scrivere codice che anticipa e affronta le situazioni in cui le cose vanno storte. I programmatori lo fanno per rendere il software robusto, prevenendo crash e comportamenti anomali.

## Come fare:

Java utilizza le eccezioni per gestire gli errori. Si circonda il codice a rischio con un blocco `try` e si intercettano le eccezioni con `catch`. Ecco un esempio semplice:

```java
public class EsempioGestioneErrori {
    public static void main(String[] args) {
        try {
            int risultato = dividi(10, 0);
            System.out.println("Il risultato è: " + risultato);
        } catch (ArithmeticException e) {
            System.out.println("Ops, non si può dividere per zero!");
        }
    }

    private static int dividi(int numeratore, int denominatore) {
        return numeratore / denominatore;
    }
}
```

Output:
```
Ops, non si può dividere per zero!
```

## Approfondimenti

La gestione degli errori in Java si è evoluta. Ai primordi non c'erano eccezioni; i programmatori controllavano i codici di errore. Poi Java ha introdotto i blocchi try-catch, permettendo una gestione degli errori più elegante.

Alternative al tradizionale `try-catch` includono `try-with-resources` per la chiusura automatica delle risorse e un codice più pulito, introdotto in Java 7.

I dettagli dell'implementazione sono importanti. Ad esempio, intercettare `Exception` o `Throwable` è generalmente una cattiva pratica. È troppo generico e cela bug di cui potresti non essere a conoscenza. È meglio attenersi a eccezioni specifiche.

## Vedi Anche

- I tutorial ufficiali di Oracle su Java e le eccezioni: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Documentazione sull'istruzione `try-with-resources` di Java: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effective Java di Joshua Bloch, per le migliori pratiche sulle eccezioni.
