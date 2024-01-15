---
title:                "Scrittura di test"
html_title:           "Gleam: Scrittura di test"
simple_title:         "Scrittura di test"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test è un'attività fondamentale nella programmazione moderna. Non solo aiuta a garantire il corretto funzionamento del codice, ma anche a facilitare l'aggiunta di nuove funzionalità e a mantenere il codice pulito e organizzato.

## Come fare

Per scrivere test efficaci in Gleam, è necessario seguire alcuni semplici passaggi.

1. Definire una funzione di test
Per prima cosa, è necessario definire una funzione di test che descriva cosa si vuole testare. Ad esempio, se si vuole testare una funzione `double` che raddoppia un numero, si può definire la funzione di test `test_double`.

```
Gleam
fn test_double() {
  assert.equal(double(2), 4)
}
```

2. Utilizzare l'asserzione appropriata
L'assertion `assert.equal` confronta il valore di output della funzione con quello atteso. Altre asserzioni disponibili includono `assert.not_equal`, `assert.true` e `assert.false`.

3. Eseguire i test
Per eseguire i test, si può utilizzare il comando `gleam test` seguito dal nome del file contenente la funzione di test.

```
$ gleam test test_double.gleam
```

4. Verificare gli output dei test
Una volta terminata l'esecuzione dei test, si otterrà un output simile a questo:

```
✓ test_double passed
```

## Approfondimento

Scrivere test efficaci in Gleam richiede una buona comprensione dei tipi di dati e delle funzioni. Inoltre, è importante ricordare di testare non solo gli scenari di successo, ma anche quelli di fallimento. Un buon approccio è implementare i test prima di scrivere il codice effettivo, in modo da poter verificare il corretto funzionamento durante lo sviluppo.

## Vedi anche

- [Documentazione ufficiale di Gleam](https://gleam.run/)
- [Articolo su come scrivere codice pulito in Gleam](https://www.lucidchart.com/techblog/2018/04/30/writing-beautiful-code-in-gleam/)