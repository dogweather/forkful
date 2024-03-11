---
date: 2024-01-26 01:11:38.304032-07:00
description: "Organizzare il codice in funzioni significa suddividere il programma\
  \ in blocchi riutilizzabili e modulari identificati da un nome. Lo facciamo per\
  \ rendere\u2026"
lastmod: '2024-03-11T00:14:16.788644-06:00'
model: gpt-4-1106-preview
summary: "Organizzare il codice in funzioni significa suddividere il programma in\
  \ blocchi riutilizzabili e modulari identificati da un nome. Lo facciamo per rendere\u2026"
title: Organizzazione del codice in funzioni
---

{{< edit_this_page >}}

## Cosa & Perché?
Organizzare il codice in funzioni significa suddividere il programma in blocchi riutilizzabili e modulari identificati da un nome. Lo facciamo per rendere il nostro codice più pulito, più leggibile e più semplice da eseguire il debug. Si tratta di non ripetere noi stessi e di ottimizzare gli aggiornamenti.

## Come fare:
Diciamo che hai del codice che calcola l'area di un cerchio più volte. Invece di ripetere la formula, la incapsuli in una funzione.

```Rust
fn calcola_area_cerchio(raggio: f64) -> f64 {
    std::f64::consts::PI * raggio.powi(2)
}

fn main() {
    let raggio = 5.0;
    let area = calcola_area_cerchio(raggio);
    println!("L'area del cerchio è: {}", area);
}
```

Output:

```
L'area del cerchio è: 78.53981633974483
```

## Approfondimento
Storicamente, le funzioni provengono dalla matematica, dove mappano gli input agli output. Nella programmazione, sono presenti fin dai tempi dell'assembly, anche se le chiamavamo 'sottoprogrammi'. Le funzioni di Rust possono restituire valori e persino altre funzioni grazie alle funzioni di prima classe e alle chiusure.

Alternative? Codice inline o macro, ma sono disordinate per logiche complesse. Gli oggetti con metodi sono un altro modo per organizzare la funzionalità, un sapore diverso rispetto alle funzioni autonome.

L'implementazione in Rust è abbastanza diretta. Le funzioni dichiarano i tipi dei loro parametri e il tipo di ritorno. Per convenzione, si usa lo 'snake case' per la denominazione. Hai le tue funzioni pubbliche (`pub fn`) per l'uso al di fuori del modulo e quelle private per l'uso interno. E Rust ha questa caratteristica interessante dove non hai bisogno di una parola chiave `return` per l'ultima espressione in una funzione.

## Vedi Anche
Consulta questi per maggiori informazioni:
- Il libro "The Rust Programming Language": [Funzioni](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust by Example sulle [Funzioni](https://doc.rust-lang.org/rust-by-example/fn.html)
