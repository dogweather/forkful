---
title:    "C++: Italiano: Convertire una data in una stringa"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Perché
Molte volte, durante la programmazione, può essere necessario convertire una data in una stringa per poterla utilizzare in modo più semplice e flessibile nel codice. Ad esempio, potresti voler visualizzare una data in un formato specifico o utilizzarla come parte di un messaggio di log.

## Come fare
Per convertire una data in una stringa in C++, puoi utilizzare la funzione `strftime` della libreria standard `<ctime>`. Questa funzione accetta due parametri: una stringa di formato che specifica come la data dovrebbe essere rappresentata e un puntatore a una struttura `tm` contenente la data stessa.

Di seguito un esempio di come utilizzare `strftime` per convertire la data attuale in una stringa nel formato `DD/MM/YYYY`:

```C++

#include <iostream>
#include <ctime>

int main()
{
    // Otteniamo la data attuale utilizzando la funzione time
    std::time_t timestamp = std::time(nullptr);

    // Convertiamo la data in una struttura tm
    std::tm date = *std::localtime(&timestamp);

    // Definiamo una stringa di formato
    // Per il formato completo dei parametri, consultare la documentazione
    const char* format = "%d/%m/%Y";

    // Utilizziamo strftime per convertire la data in una stringa
    // Nota che il parametro size indica la dimensione massima della stringa
    // Da specificare in modo da evitare overflow
    char stringa_data[11];
    std::strftime(stringa_data, 11, format, &date);

    // Stampiamo la stringa ottenuta
    std::cout << stringa_data << std::endl;

    return 0;
}

// Output: 23/07/2021
```

## Approfondimento
Ci sono molti altri formati che è possibile utilizzare nella stringa di formato. Ad esempio, per ottenere il nome del mese anziché il suo numero, è possibile utilizzare `%B` al posto di `%m`.

Inoltre, la funzione `strftime` è solo una delle opzioni per convertire una data in una stringa. Altri modi includono l'utilizzo di librerie esterne o scrivere una funzione personalizzata per gestire la conversione.

## Vedi anche
- [Documentazione ufficiale di strftime](https://www.cplusplus.com/reference/ctime/strftime/)
- [Come ottenere la data e l'ora attuali in C++](https://www.learncpp.com/cpp-tutorial/514-date-and-time/)
- [Convertire una data in una stringa con FormatDate](https://www.techiedelight.com/convert-date-string-cpp/)