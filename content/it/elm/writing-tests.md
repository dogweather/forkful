---
title:    "Elm: Scrivere test di programmazione"
keywords: ["Elm"]
---

{{< edit_this_page >}}

##Perché

Scrivere test è una pratica fondamentale nella programmazione Elm. Non solo aiuta a garantire che il nostro codice funzioni come previsto, ma ci dà anche la fiducia necessaria per apportare modifiche in modo sicuro e senza temere di introdurre errori.

##Come fare

Per iniziare a scrivere test in Elm, dobbiamo utilizzare il pacchetto `elm-test` disponibile su Elm Package Manager. Una volta installato, possiamo creare un file `tests/Tests.elm` che conterrà tutti i nostri test.

Diamo uno sguardo ad un semplice esempio di test in Elm:

```elm
module Tests exposing (..)

import Expect
import Main exposing (..)

sumTest =
    test "Sum function should return correct result" <|
        \_ ->
            Expect.equal (sum 2 3) 5
```

Notare che dobbiamo importare il modulo `Expect` per utilizzare il suo metodo `equal`, che confronta il nostro valore di aspettativa con quello ricevuto dalla funzione che stiamo testando.

Ora che abbiamo creato un test, dobbiamo eseguirlo. Ciò può essere fatto tramite il comando `elm-test` da terminale, che eseguirà tutti i test disponibili nel nostro file `Tests.elm`.

Inoltre, possiamo utilizzare il modulo `Test` per creare test più complessi e per gestire eventuali errori e assertion personalizzati. Per ulteriori informazioni sui moduli da utilizzare per i test in Elm, si consiglia di consultare la documentazione ufficiale.

##Approfondimento

In aggiunta alla scrittura dei test, c'è una pratica comune nella programmazione Elm che è nota come **test-driven development** (TDD). Questo approccio prevede di scrivere i test prima di scrivere il codice effettivo, in modo da avere una guida chiara su cosa il nostro codice dovrebbe fare.

Inoltre, la scrittura dei test ci aiuta a scrivere un codice più modulare e robusto. Quando scriviamo test, siamo costretti a pensare ai vari casi limite e alle possibili eccezioni del nostro codice, rendendolo più sicuro e gestibile.

Inoltre, la pratica di scrivere test ci porta a scrivere codice più leggibile e ben strutturato, poiché dobbiamo essere sicuri di capire veramente cosa il nostro codice sta facendo.

##Vedi anche

- [Documentazione ufficiale di Elm sul testing](https://guide.elm-lang.org/testing/)
- [Pacchetto elm-test su Elm Package Manager](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Articolo su test-driven development in Elm](https://www.brianthicks.com/post/2019/08/13/test-driven-development-in-elm/)