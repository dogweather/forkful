---
title:                "C++: Calcolare una data nel futuro o nel passato"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Spesso nei nostri programmi, abbiamo bisogno di lavorare con date e calcolare una data futura o passata in base alle nostre esigenze. In questo articolo, parleremo di come possiamo fare ciò utilizzando il linguaggio di programmazione C++.

## Come fare
Per calcolare una data futura o passata, dobbiamo prima creare un oggetto della classe `tm` che rappresenta una data e utilizzare la funzione `mktime()` per convertirlo in secondi dall'epoca Unix. Utilizzando questa funzione, possiamo anche aggiungere o sottrarre un certo numero di secondi dalla data originale per ottenere la data desiderata. Di seguito è riportato un esempio di codice che calcola la data di domani utilizzando questa tecnica:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // Creiamo un oggetto della classe tm inizializzato con la data odierna
    time_t now = time(0);
    tm *date = localtime(&now);

    // Otteniamo l'epoca Unix della data di domani
    date->tm_mday += 1;
    time_t tomorrow = mktime(date);

    // Stampiamo la data di domani
    cout << "Domani è il " << ctime(&tomorrow) << endl;

    return 0;
}
```

Questo programma stamperà qualcosa del genere:

```
Domani è il (giorno della settimana) (mese) (giorno) (ora) (fuso orario) (anno)
```

Possiamo anche utilizzare la stessa tecnica per calcolare una data passata. Dobbiamo solo sottrarre i secondi desiderati dalla data originale. Di seguito è riportato un esempio di codice per ottenere la data di ieri:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // Creiamo un oggetto della classe tm inizializzato con la data odierna
    time_t now = time(0);
    tm *date = localtime(&now);

    // Otteniamo l'epoca Unix della data di ieri
    date->tm_mday -= 1;
    time_t yesterday = mktime(date);

    // Stampiamo la data di ieri
    cout << "Ieri era il " << ctime(&yesterday) << endl;

    return 0;
}
```

Questo programma stamperà qualcosa del genere:

```
Ieri era il (giorno della settimana) (mese) (giorno) (ora) (fuso orario) (anno)
```

## Approfondimento
Oltre alla funzione `mktime()`, il linguaggio C++ offre anche altre funzioni utili per lavorare con le date, come ad esempio `time()` per ottenere l'ora corrente, `localtime()` per convertire l'epoca Unix in un oggetto della classe tm e `strftime()` per formattare la data e l'ora secondo le nostre esigenze.

Inoltre, è possibile utilizzare la libreria `<chrono>` per calcolare date e tempi in maniera più semplice ed elegante. Questa libreria è stata introdotta nel C++11 e offre un'ottima alternativa per lavorare con le date. Di seguito è riportato un esempio di codice che calcola la data di domani utilizzando la libreria `<chrono>`:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

using namespace std;
using namespace std::chrono;

int main() {
    // Otteniamo il tempo corrente
    system_clock::time_point now = system_clock::now();
    // Aggiungiamo un giorno al tempo corrente
    now += duration<int, ratio<60*60*24>>(1);
    // Convertiamo il tempo in una data
    time_t tomorrow = system_clock::to_time_t(now);
    // Stampiamo la data di domani
    cout << ctime(&tomorrow) << endl;

    return 0;
}
```

Questo programma stamperà la stessa cosa del primo esempio, ma in questo caso abbiamo utilizzato la libreria `<chrono>` per calcolare la data di domani in modo più semplice e intuitivo.

## Vedi anche
- [Documentazione sulla classe tm](https