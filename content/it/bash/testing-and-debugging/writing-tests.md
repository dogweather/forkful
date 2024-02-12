---
title:                "Scrivere test"
aliases:
- /it/bash/writing-tests/
date:                  2024-02-03T19:29:32.489874-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere test"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e perché?
Scrivere test in Bash comporta la creazione di script per casi di test al fine di convalidare la funzionalità dei vostri script Bash. I programmatori conducono test per assicurarsi che i loro script funzionino come previsto in varie condizioni, intercettando errori e bug prima del rilascio.

## Come fare:
Bash non ha un framework di test integrato, ma è possibile scrivere semplici funzioni di test. Per test più sofisticati, strumenti di terze parti come `bats-core` sono popolari.

### Esempio di test di base in Bash puro:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "Test superato."
    return 0
  else
    echo "Test fallito. Aspettato '$expected_output', ottenuto '$result'"
    return 1
  fi
}

# Invocazione della funzione di test
test_example_function
```
Output dell'esempio:
```
Test superato.
```

### Usare `bats-core` per i test:
Prima, installare `bats-core`. Questo può di solito essere fatto tramite il gestore di pacchetti o clonando il suo repository.

Poi, scrivere i vostri test in file `.bats` separati.

```bash
# File: example_function.bats

#!/usr/bin/env bats

@test "testare la funzione di esempio" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
Per eseguire i test, basta eseguire il file `.bats`:
```bash
bats example_function.bats
```
Output dell'esempio:
```
 ✓ testare la funzione di esempio

1 test, 0 fallimenti
```

Questo approccio consente di integrare facilmente i test nel vostro flusso di lavoro di sviluppo, garantendo l'affidabilità e la stabilità degli script Bash.
