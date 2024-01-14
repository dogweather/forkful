---
title:    "Elixir: Scrivere test"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché scrivere test in Elixir?

Scrivere test può sembrare un'attività noiosa e aggiuntiva quando si è impegnati nella programmazione in Elixir. Tuttavia, i test sono uno strumento importante per garantire che il nostro codice funzioni correttamente e sia robusto. Non solo ci aiutano a trovare e risolvere eventuali errori, ma ci permettono anche di essere più sicuri della qualità del nostro codice.

## Come scrivere test in Elixir

Per iniziare a scrivere test in Elixir, dobbiamo prima importare il modulo `ExUnit` nel nostro file. Ciò ci permette di utilizzare le funzioni di testing di Elixir. Successivamente, creiamo una funzione di test utilizzando il blocco `test do ... end`. All'interno di questo blocco, possiamo utilizzare le funzioni di test `assert` e `refute` per verificare l'output del nostro codice.

```Elixir
import ExUnit

# Funzione di test
test "aggiunta di due numeri" do
  assert 2 + 2 === 4
end
```

Possiamo anche eseguire test su funzioni e moduli specifici utilizzando il blocco `describe ... do` e `doctest ... end`. Inoltre, possiamo utilizzare la funzione `setup` per definire codice da eseguire prima di ogni test e la funzione `teardown` per definire codice da eseguire dopo ogni test.

```Elixir
# Test su una funzione specifica
describe "doppio" do
  test "ritorna il doppio di un numero" do
    assert MioModulo.doppio(3) === 6
  end
end

# Test degli esempi di utilizzo di una funzione
doctest MioModulo.doppio
```

## Approfondimento sui test in Elixir

Ci sono molti strumenti e tecniche disponibili per scrivere test in maniera più efficiente ed efficace in Elixir. Ad esempio, Elixir ci permette di utilizzare `ExUnit.Case` per raggruppare i nostri test e di utilizzare `ExUnit.CaptureIO` per testare l'output di codice che genera output.

Inoltre, esistono librerie come `Bypass` e `Mox` che ci consentono di testare il codice che effettua chiamate HTTP e di mocking delle funzioni, rispettivamente.

In generale, è importante scrivere test che siano leggibili, mantenibili e che coprano tutti i casi possibili. Ciò ci aiuterà a trovare e risolvere bug più velocemente e a garantire la qualità del nostro codice.

## Vedi anche

- [Documentazione di ExUnit](https://hexdocs.pm/ex_unit/)
- [Bypass: Libreria per testare chiamate HTTP in Elixir](https://github.com/pspdfkit-labs/bypass)
- [Mox: Libreria per il mocking delle funzioni in Elixir](https://github.com/plataformatec/mox)