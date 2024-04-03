---
date: 2024-01-26 03:37:10.132426-07:00
description: "Come fare: Supponiamo di avere un blocco di codice che calcola e stampa\
  \ l'area e il perimetro di un rettangolo dato il suo lato e larghezza. Fa il suo\u2026"
lastmod: '2024-03-13T22:44:43.009390-06:00'
model: gpt-4-0125-preview
summary: Supponiamo di avere un blocco di codice che calcola e stampa l'area e il
  perimetro di un rettangolo dato il suo lato e larghezza.
title: Rifattorizzazione
weight: 19
---

## Come fare:
Supponiamo di avere un blocco di codice che calcola e stampa l'area e il perimetro di un rettangolo dato il suo lato e larghezza. Fa il suo lavoro, ma è ripetitivo e un po' disordinato.

```python
# Versione Originale
lunghezza = 4
larghezza = 3

# Calcola area e perimetro
area = lunghezza * larghezza
perimetro = 2 * (lunghezza + larghezza)

print("Area:", area)
print("Perimetro:", perimetro)
```

Possiamo rifattorizzare questo incapsulando la funzionalità in funzioni, rendendo il codice più organizzato e riutilizzabile:

```python
# Versione Rifattorizzata

def calcola_area(lunghezza, larghezza):
    return lunghezza * larghezza

def calcola_perimetro(lunghezza, larghezza):
    return 2 * (lunghezza + larghezza)

# uso
lunghezza = 4
larghezza = 3

print("Area:", calcola_area(lunghezza, larghezza))
print("Perimetro:", calcola_perimetro(lunghezza, larghezza))
```

Entrambi gli snippet producono lo stesso risultato:
```
Area: 12
Perimetro: 14
```

Ma la versione rifattorizzata è più pulita e separa le preoccupazioni, rendendo più facile aggiornare un calcolo senza influenzare l'altro.

## Approfondimento
Il refactoring ha le sue radici nei primi giorni dell'ingegneria del software, quando i programmatori si sono resi conto che il codice poteva — e doveva — essere migliorato anche se già "funzionante". Il libro fondamentale di Martin Fowler "Refactoring: Improving the Design of Existing Code" ha articolato molti principi e tecniche fondamentali. Lui ha famosamente detto: "Qualsiasi sciocco può scrivere codice che un computer può capire. I bravi programmatori scrivono codice che gli umani possono capire."

Le alternative al refactoring potrebbero includere la riscrittura del codice da zero o fare piccoli aggiustamenti senza miglioramento sistematico. Tuttavia, il refactoring è di solito più conveniente di una riscrittura e meno rischioso di modifiche ad-hoc. I dettagli di implementazione possono essere specifici per ciascun paradigma di programmazione; tuttavia, la programmazione orientata agli oggetti si presta particolarmente bene al refactoring, specialmente con tecniche come l'estrazione di metodi (come le nostre funzioni `calcola_area` e `calcola_perimetro`), l'inlining, lo spostamento di funzionalità tra oggetti e la rinominazione di metodi o variabili per chiarezza.

Il refactoring in Python spesso utilizza strumenti come `PyCharm`, che ha capacità di refactoring integrate, o `rope`, una libreria Python specificamente progettata per il refactoring. Si consiglia vivamente l'uso attento del controllo versione, come `git`, durante il refactoring per tenere traccia dei cambiamenti in modo incrementale.

## Vedere Anche
Per chi desidera approfondire, esplorare le seguenti risorse:
- Il libro di Martin Fowler: [Refactoring: Improving the Design of Existing Code](http://www.refactoring.com/)
- Refactoring in Python con `rope`: [GitHub - rope](https://github.com/python-rope/rope)
- Documentazione sul refactoring di PyCharm: [Jetbrains PyCharm Refactoring Source Code](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [Refactoring and Design Patterns](https://refactoring.guru/refactoring)
- Lezioni su Clean Code di Zio Bob (Robert C. Martin): [Clean Code - Zio Bob / Lezione 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
