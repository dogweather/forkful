---
date: 2024-01-26 04:36:58.626099-07:00
description: "I numeri complessi hanno una parte reale e una immaginaria, tipicamente\
  \ scritti come `a + bi`. Sono vitali per alcuni progetti Arduino pesanti dal punto\u2026"
lastmod: '2024-03-11T00:14:17.296166-06:00'
model: gpt-4-0125-preview
summary: "I numeri complessi hanno una parte reale e una immaginaria, tipicamente\
  \ scritti come `a + bi`. Sono vitali per alcuni progetti Arduino pesanti dal punto\u2026"
title: Lavorare con i numeri complessi
---

{{< edit_this_page >}}

## Cosa & Perché?
I numeri complessi hanno una parte reale e una immaginaria, tipicamente scritti come `a + bi`. Sono vitali per alcuni progetti Arduino pesanti dal punto di vista matematico che coinvolgono l'elaborazione di segnali, l'ingegneria elettrica, o qualsiasi altro dominio dove i fenomeni sono meglio modellati in un piano.

## Come fare:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // Avvia la comunicazione seriale
  
  Complex myComplex(2, 3); // Crea un numero complesso 2 + 3i
  Complex anotherComplex(1, 1); // Crea un altro numero complesso 1 + 1i
  
  // Addizione
  Complex result = myComplex + anotherComplex; 
  Serial.print("Addizione: "); 
  result.print(); // Mostra 3 + 4i
  
  // Moltiplicazione
  result = myComplex * anotherComplex; 
  Serial.print("Moltiplicazione: ");
  result.print(); // Mostra -1 + 5i
}

void loop() {
  // Non usato in questo esempio
}
```
Output di esempio:
```
Addizione: 3 + 4i
Moltiplicazione: -1 + 5i
```

## Approfondimento
In origine, i numeri complessi furono accolti con scetticismo, ma sono diventati centrali in vari campi scientifici. Storicamente, sono stati riconosciuti per fornire soluzioni a equazioni polinomiali che mancano di soluzioni reali.

Arduino non include i numeri complessi nella sua libreria standard, ma è possibile sfruttare librerie come `Complex.h` per gestirli. Internamente, queste librerie definiscono una classe Complex, tipicamente utilizzando due double per memorizzare le parti reale e immaginaria, e sovraccaricano gli operatori per supportare l'aritmetica.

Come alternativa, per applicazioni che non necessitano intrinsecamente dell’aritmetica dei numeri complessi, considerate l'uso di altre strategie matematiche o librerie. Ricordate, tuttavia, che usare i float invece dei numeri complessi potrebbe semplificare eccessivamente alcuni problemi.

## Vedi Anche
- La libreria [Complex.h](https://github.com/RobTillaart/Complex) di Rob Tillaart.
- Un approfondimento sulla [matematica dietro i numeri complessi](https://mathworld.wolfram.com/ComplexNumber.html).
