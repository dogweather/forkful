---
title:                "Gleam: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test è importante in Gleam

Scrivere test è un aspetto fondamentale nella programmazione, in particolare quando si utilizza un linguaggio più nuovo come Gleam. I test ci aiutano a verificare che il nostro codice funzioni come previsto e aiutano anche a identificare e risolvere eventuali errori più facilmente. Inoltre, avere una buona suite di test può aumentare la fiducia nel nostro codice e rendere il processo di sviluppo più efficiente.

## Come scrivere test in Gleam

In Gleam, possiamo scrivere test utilizzando il modulo `gleam_test`. Per prima cosa, dobbiamo importare il modulo nel nostro file di test. Quindi possiamo definire una funzione di test utilizzando il costrutto `@test` e passare come argomento l'espressione che vogliamo testare. Ad esempio:

```Gleam
@test(2 + 2) = 4
@test(3 * 3) = 9
```

Possiamo anche utilizzare il costrutto `@test` in una funzione di test separata e quindi richiamare questa funzione all'interno di un'altra funzione `@test`. Questo è utile se abbiamo più test da eseguire che condividono la stessa logica. Un esempio di questo potrebbe essere:

```Gleam
fn sum(a, b) {
  a + b
}

fn test_sum() {
  assert(sum(2,2)) = 4
  assert(sum(3,3)) = 6
}

@test("2+2=4") {
  test_sum() |> assert
}
```

Per eseguire i nostri test, possiamo utilizzare il comando `gleam test` dalla nostra cartella di progetto. Se tutto va bene, dovremmo vedere un output di questo genere:

```
Compiling...

 12 tests passed

00:00:00 ✓
```

## Approfondimenti sui test in Gleam

Oltre agli esempi di base forniti sopra, ci sono molti altri modi in cui possiamo scrivere test in Gleam. Possiamo ad esempio utilizzare gli assert per verificare se una condizione è vera o falsa, o utilizzare il modulo `gleam_assert` per gestire gli errori dei nostri test in modo più specifico. Inoltre, possiamo utilizzare il costrutto `@setup` per eseguire alcune operazioni prima di ogni funzione `@test` o il costrutto `@before_suite` per eseguire alcune operazioni una sola volta prima di tutti i test.

Una fonte utile per ulteriori approfondimenti sui test in Gleam è la documentazione ufficiale, dove vengono spiegati dettagliatamente tutti i costrutti e i moduli disponibili per scrivere test.

## Vedi anche

- Documentazione ufficiale sui test in Gleam: https://gleam.run/book/testing.html
- Esempi di test in Gleam: https://github.com/gleam-lang/gleam/blob/master/examples/test
- Tutorial su come scrivere test in Gleam: https://gleam.run/book/unit_testing.html