---
title:                "Scrivere test"
html_title:           "PHP: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test?

Scrivere test è fondamentale per garantire la qualità del codice e risparmiare tempo nel processo di sviluppo. I test permettono di individuare errori in modo tempestivo, facilitare la manutenzione del codice e garantire che le nuove funzionalità non rompano quelle già esistenti.

## Come scrivere test in PHP

Per iniziare a scrivere test in PHP, è consigliato utilizzare un framework di testing come PHPUnit. Questo framework permette di creare e gestire i test in modo semplice e organizzato.

Nella seguente sezione, vedremo come creare e eseguire un semplice test utilizzando PHPUnit.

```PHP
// Include il file dove è definita la classe da testare
require_once 'src/EsempioClasse.php';

// Importa la classe PHPUnitTestCase
use PHPUnit\Framework\TestCase;

class EsempioClasseTest extends TestCase
{
    // Metodo per verificare che la classe sia istanziata correttamente
    public function testClassCreation()
    {
        $esempio = new EsempioClasse();

        // Verifica che l'istanza sia della classe EsempioClasse
        $this->assertInstanceOf(EsempioClasse::class, $esempio);
    }

    // Metodo per verificare il corretto funzionamento di un metodo della classe
    public function testExampleMethod()
    {
        $esempio = new EsempioClasse();

        // Invoca il metodo da testare
        $result = $esempio->esempioMetodo();

        // Verifica che il risultato sia quello atteso
        $this->assertEquals('Esempio', $result);
    }
}
```

Una volta creati i test, per eseguirli è sufficiente utilizzare il comando `phpunit` seguito dal nome del file che contiene i test.

## Approfondimento

Oltre ai test di unità, è importante includere anche i test di integrazione, che verificano il corretto funzionamento di più componenti del sistema. E' inoltre consigliabile utilizzare il principio di test-driven development, dove si scrivono prima i test e poi il codice per soddisfare quei test.

## Vedi anche

- [Guida a PHPUnit (in italiano)](https://phpunit.de/manual/8.0/it/index.html)
- [Principi del test-driven development](https://www.agilealliance.org/glossary/tdd/)