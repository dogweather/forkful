---
title:    "C++: Scrivere su standard error"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Perché

Scrivere messaggi di errore è una parte essenziale della programmazione in C++. Fornire messaggi di errore chiari e precisi aiuta a identificare e risolvere eventuali problemi nel codice.

##Come fare

Per scrivere un messaggio di errore in C++, è necessario utilizzare la funzione `std::cerr` dalla libreria standard. Questa funzione invia un messaggio di errore al flusso di output standard di errore.

Ecco un esempio di come utilizzare `std::cerr` per scrivere un messaggio di errore:

```C++
#include <iostream>

int main()
{
    int age = -5;

    // Condizione per il controllo dell'età
    if (age < 0) {
        std::cerr << "Errore: l'età non può essere un numero negativo." << std::endl;
        return -1;
    }
    
    return 0;
}
```

Ecco l'output che riceverai se esegui questo programma:

```
Errore: l'età non può essere un numero negativo.
```

Come puoi vedere, il messaggio di errore viene stampato sullo standard error. Utilizzando `std::cerr`, possiamo specificare la causa dell'errore e fornire una descrizione dettagliata per aiutare a risolvere il problema.

##Approfondimento

Scrivere messaggi di errore è importante non solo per identificare e risolvere problemi, ma anche per rendere il tuo codice più robusto e sicuro. Quando si lavora su progetti collaborativi, fornire messaggi di errore ben scritti può aiutare gli altri membri del team a capire il codice e a risolvere eventuali problemi più rapidamente.

Inoltre, esistono altre funzioni nella libreria standard, come `std::cout` e `std::clog`, che possono essere utilizzate per scrivere su diversi flussi di output. Ad esempio, `std::cout` viene utilizzato per stampare su stdout (standard output), mentre `std::clog` viene utilizzato per stampare su un file di log.

Inoltre, è importante notare che quando si utilizza `std::cerr`, il flusso viene automaticamente pulito prima di essere utilizzato, quindi non è necessario aggiungere un carattere di nuova linea alla fine del messaggio (come è stato fatto nell'esempio sopra).

##Vedi anche

- [Documentazione ufficiale di C++ su `std::cerr`](https://en.cppreference.com/w/cpp/io/cerr)
- [Come scrivere messaggi di errore efficaci in C++ (in inglese)](https://www.fluentcpp.com/2017/01/05/std-err-why-your-console-program-is-important/)

Grazie per aver letto questo breve articolo sulle basi di come scrivere a standard error in C++. Ricorda che fornire messaggi di errore accurati è fondamentale per scrivere codice di qualità e per risolvere i problemi in modo efficiente. Continua a praticare e presto sarai un maestro nell'utilizzo di `std::cerr` e altre funzioni di output!