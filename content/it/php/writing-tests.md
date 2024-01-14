---
title:    "PHP: Scrivere test"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test è una pratica fondamentale per uno sviluppatore PHP professionista. Non solo aiuta a garantire il corretto funzionamento del codice, ma anche a facilitare il processo di debugging e a migliorare la qualità del software.

## Come fare

Per scrivere test in PHP, è necessario utilizzare un framework di testing come PHPUnit. Di seguito viene mostrato un esempio di codice di un test di unità per una semplice funzione di somma:

```PHP
public function testSomma()
{
    $risultato = somma(2, 3);
    $this->assertEquals(5, $risultato);
}
```

Questa funzione testa se la funzione "somma" restituisce correttamente il risultato atteso, ovvero 5, quando chiamata con gli argomenti 2 e 3. Il codice all'interno del blocco ```PHP ... ``` viene eseguito come codice PHP normale, ma le affermazioni come "assertEquals" sono metodi di PHPUnit specifici per i test.

## Deep Dive

Scoprire tutte le funzionalità e i metodi disponibili in PHPUnit richiede del tempo e della pratica, ma ci sono alcune best practice da tenere a mente per scrivere test efficaci:

- Scrivere test prima di scrivere il codice effettivo (TDD - Test Driven Development).
- Testare tutte le funzionalità e i casi limite possibili.
- Utilizzare nomi significativi per i test e gli output dei messaggi di errore per facilitare il debugging.
- Rimuovere i test inutili o ridondanti per mantenere la suite di test snella e facile da mantenere.

## Vedi anche

- [PHPUnit Documentation](https://phpunit.de/documentation.html)
- [PHP Testing: Beginner's Guide](https://code.tutsplus.com/tutorials/php-testing-beginners-guide--net-5144)
- [The Art of Unit Testing with Examples in PHP](https://www.amazon.com/Art-Unit-Testing-Examples/dp/1617291502)