---
date: 2024-01-27 20:32:33.107677-07:00
description: "Generare numeri casuali nei progetti Arduino implica la produzione di\
  \ valori che sono imprevedibili per progettazione, fondamentale per applicazioni\
  \ come\u2026"
lastmod: '2024-03-13T22:44:43.679402-06:00'
model: gpt-4-0125-preview
summary: Generare numeri casuali nei progetti Arduino implica la produzione di valori
  che sono imprevedibili per progettazione, fondamentale per applicazioni come giochi,
  simulazioni e sistemi di sicurezza.
title: Generazione di numeri casuali
weight: 12
---

## Come fare:
Arduino fornisce funzioni semplici per generare numeri casuali: `randomSeed()` e `random()`. Per iniziare, inizializza il generatore di numeri casuali per assicurare diverse sequenze di numeri ogni volta che il tuo programma viene eseguito. Un approccio spesso utilizzato è quello di inizializzare con una lettura analogica da un pin non connesso.

```Arduino
void setup() {
  Serial.begin(9600);
  // Inizializza il seme casuale
  randomSeed(analogRead(0));
}

void loop() {
  // Genera un numero casuale tra 0 e 99
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // Ritardo di un secondo per la leggibilità dell'output
}
```

Il programma sopra inizializza il generatore di numeri casuali nella funzione `setup()` e genera un nuovo numero tra 0 e 99 in ogni iterazione del loop, trasmettendo il numero al Monitor Seriale.

Output di esempio:
```
42
17
93
...
```

## Approfondimento
La funzione `random()` di Arduino, sotto il cofano, sfrutta un generatore di numeri pseudo-casuali (PRNG), che segue una sequenza deterministica ma appare statisticamente casuale. Il valore iniziale, o seme, della sequenza influenza fortemente la sua imprevedibilità, da qui l'uso comune di `randomSeed()` con un input in qualche modo casuale come punto di partenza. È importante notare che la casualità generata da Arduino è sufficiente per la maggior parte dei progetti amatoriali ma potrebbe non rispettare i criteri per applicazioni ad alta sicurezza a causa della sua prevedibilità nel tempo. Per scopi crittografici, è consigliabile esplorare algoritmi più sofisticati e generatori di numeri casuali hardware (HRNGs), che possono fornire vera casualità utilizzando processi fisici.
