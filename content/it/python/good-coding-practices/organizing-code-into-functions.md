---
date: 2024-01-26 01:16:11.990763-07:00
description: "Organizzare il codice in funzioni consiste nel suddividere il codice\
  \ in blocchi riutilizzabili con scopi specifici. Lo facciamo per rendere il codice\
  \ pi\xF9\u2026"
lastmod: '2024-03-13T22:44:43.006562-06:00'
model: gpt-4-0125-preview
summary: Organizzare il codice in funzioni consiste nel suddividere il codice in blocchi
  riutilizzabili con scopi specifici.
title: Organizzare il codice in funzioni
weight: 18
---

## Come fare:
Diciamo che stai scrivendo uno script per calcolare il quadrato e il cubo di un numero. Senza funzioni, è un pasticcio di ripetizioni:

```Python
num = 4
quadrato = num * num
cubo = num * num * num
print(f"Quadrato: {quadrato}, Cubo: {cubo}")

num = 5
quadrato = num * num
cubo = num * num * num
print(f"Quadrato: {quadrato}, Cubo: {cubo}")
```
Output:
```
Quadrato: 16, Cubo: 64
Quadrato: 25, Cubo: 125
```

Con le funzioni, è più ordinato:

```Python
def quadrato(n):
    return n * n

def cubo(n):
    return n ** 3

num = 4
print(f"Quadrato: {quadrato(num)}, Cubo: {cubo(num)}")

num = 5
print(f"Quadrato: {quadrato(num)}, Cubo: {cubo(num)}")
```
Output:
```
Quadrato: 16, Cubo: 64
Quadrato: 25, Cubo: 125
```

## Approfondimento
Nei vecchi tempi, quando i programmi erano semplici, si poteva cavarsela scrivendo solo una lista di istruzioni. Ma man mano che il software diventava più complesso, gli sviluppatori si sono resi conto che stavano riscrivendo lo stesso codice ancora e ancora. Ecco quindi le funzioni: blocchi di codice riutilizzabili che eseguono una singola azione.

Alternative alle funzioni includono classi (raggruppamento di funzioni con i dati su cui operano) e codice inline (intelligenza proprio dove serve, ma rischioso per compiti complessi). Dal punto di vista dell'implementazione, l'astuzia non è solo creare funzioni, ma farle fare bene una cosa sola—pensa al principio di responsabilità unica. Le funzioni dovrebbero idealmente anche essere senza stato, nel senso che non ci sono sorprese con dati in entrata o in uscita.

## Vedi Anche
- I tutorial ufficiali di Python sulle funzioni: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 'Clean Code' di Robert C. Martin, per i principi su come scrivere funzioni pulite.
- 'Refactoring: Migliorare il design del codice esistente' di Martin Fowler, che include esempi di organizzazione del codice.
