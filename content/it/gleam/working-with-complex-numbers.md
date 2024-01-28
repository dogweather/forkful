---
title:                "Lavorare con i numeri complessi"
date:                  2024-01-26T04:40:31.130833-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i numeri complessi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
I numeri complessi hanno una parte reale e una parte immaginaria (`a + bi`). Sono utili in vari campi come l'ingegneria elettrica e il calcolo quantistico. I programmatori li usano per modellare equazioni che non sono risolvibili usando solo numeri reali.

## Come fare:
Gleam non supporta nativamente i numeri complessi. Di solito, si crea una propria soluzione o si cerca una libreria. Ecco un rapido esempio di come si potrebbero implementare le operazioni di base:

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let somma = add(num1, num2)
  let prodotto = multiply(num1, num2)

  somma // Complex(4.0, 6.0)
  prodotto // Complex(-5.0, 10.0)
}
```

## Approfondimento

I numeri complessi sono stati documentati più formalmente per la prima volta da Gerolamo Cardano nel XVI secolo. Sono un'estensione naturale dei numeri reali. Tuttavia, in un linguaggio giovane come Gleam, che privilegia le prestazioni e la sicurezza dei tipi, tali funzionalità sono essenziali (o si realizzano da soli).

In alcuni altri linguaggi, come Python, i numeri complessi sono integrati (`3+4j`), facilitando le cose. In Rust o Haskell, esistono librerie che offrono funzionalità avanzate pronte all'uso.

L'approccio di Gleam significa che devi occuparti di tutti gli aspetti: aritmetica, coordinate polari, forme esponenziali, ecc. Implementare operazioni efficienti e accurate richiede una programmazione attenta, considerando come il comportamento dei numeri in virgola mobile possa influenzare i tuoi risultati.

Ricorda di testare a fondo, soprattutto i casi limite! Gestire l'infinito complesso e i valori NaN (not a number) può causare problemi se non si è attenti.

## Vedere Anche
Per ulteriori informazioni, ecco dove puoi approfondire:
- [Documentazione Ufficiale di Gleam](https://gleam.run/documentation/)
- Esplora le librerie di altri linguaggi per trarre ispirazione, come il [num-complex](https://crates.io/crates/num-complex) di Rust o il [modulo cmath](https://docs.python.org/3/library/cmath.html) di Python.
