---
title:                "Scrivere test"
html_title:           "C#: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Scrivere test è un modo per verificare che il nostro codice funzioni correttamente. I programmatori lo fanno per garantire la qualità del loro software e per prevenire errori futuri.

## Come fare:
Di seguito è riportato un esempio di codice che mostra come scrivere un test in C# utilizzando il framework di test NUnit. 

```C#
[Test]
public void TestMethod()
{
    // Arrange: inizializziamo gli oggetti necessari
    var calculator = new Calculator();

    // Act: eseguiamo il metodo da testare
    var result = calculator.Add(2, 3);

    // Assert: verifichiamo che l'output sia quello atteso
    Assert.AreEqual(5, result);
}
```

L'output del test sarà "Pass" se tutti gli Assert sono verificati con successo. In caso contrario, verrà mostrato un messaggio di errore che aiuterà a identificare il problema nel codice.

## Approfondimenti:
Scrivere test è una buona pratica di programmazione e viene spesso utilizzata nel contesto dello sviluppo agile. Ci sono diverse alternative al framework di test NUnit come xUnit e MSTest. Inoltre, i test possono essere scritti a vari livelli di granularità (unit test, integration test, end-to-end test) a seconda delle esigenze del progetto.

## Vedi anche:
- [Tutorial su test in C# con NUnit](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-nunit)