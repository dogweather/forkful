---
title:                "C++: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

Ciao amici programmatori!

Siete alla ricerca di un modo per semplificare la ricerca e la manipolazione di testo all'interno dei vostri programmi? Allora state leggendo l'articolo giusto! Parleremo delle espressioni regolari, un potente strumento utilizzato nella programmazione per riconoscere e manipolare pattern all'interno di una stringa di testo. Continuate a leggere per scoprire il perché, il come e una panoramica più dettagliata sull'utilizzo delle espressioni regolari in C++.

## Perché

Le espressioni regolari sono uno strumento fondamentale per semplificare la gestione del testo all'interno dei nostri programmi. Grazie ad esse, possiamo trovare e manipolare porzioni di testo in modo molto preciso e veloce. Questo è particolarmente utile quando si lavora con grandi quantità di dati o quando si deve scrivere codice efficiente e performante.

## Come utilizzare le espressioni regolari in C++

Per utilizzare le espressioni regolari in C++, è necessario utilizzare la libreria standard `regex`. Questa libreria offre una serie di funzioni e metodi per creare, analizzare e manipolare espressioni regolari. Vediamo un esempio di codice che utilizza espressioni regolari per trovare una parola all'interno di una stringa:

```C++
#include <iostream>
#include <regex> // includiamo la libreria regex

using namespace std;

int main() {
    string testo = "Ciao amici programmatori!";
    regex espressione("amici"); // creiamo una nuova espressione regolare
    smatch risultati; // crea un oggetto di tipo smatch per immagazzinare i risultati

    if (regex_search(testo, risultati, espressione)) { // utilizziamo la funzione regex_search per trovare la parola "amici"
        cout << "Parola trovata: " << risultati[0] << endl; // stampiamo il risultato
    } else {
        cout << "Parola non trovata." << endl;
    }

    return 0;
}
```

L'output di questo codice sarà: `Parola trovata: amici`. Come possiamo vedere, le espressioni regolari ci hanno permesso di trovare la parola "amici" all'interno della stringa di testo in modo molto semplice e veloce.

## Approfondimento sulle espressioni regolari

Le espressioni regolari non sono solo utili per trovare parole specifiche all'interno di una stringa, ma possono anche essere utilizzate per validare formati di dati, sostituire porzioni di testo e molto altro. La loro sintassi può sembrare complicata all'inizio, ma una volta imparate alcune regole fondamentali, diventano uno strumento estremamente potente.

Le espressioni regolari sono una combinazione di caratteri che rappresentano un particolare pattern di testo da cercare. Ad esempio, il carattere `\d` indica qualsiasi cifra da 0 a 9, mentre il carattere `.` indica qualsiasi carattere singolo. Utilizzando una combinazione di questi caratteri, possiamo creare espressioni regolari per trovare numeri di telefono, indirizzi email, codici postali e molto altro ancora.

Una delle funzioni più utilizzate delle espressioni regolari è la sostituzione di porzioni di testo. Ad esempio, se volessimo sostituire tutte le vocali di una stringa con asterischi, possiamo utilizzare il seguente codice:

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
    string testo = "Ciao amici programmatori!";
    regex espressione("[aeiou]"); // tutti i caratteri "a", "e", "i", "o" o "u"
    string risultato = regex_replace(testo, espressione, "*"); // la funzione regex_replace sostituisce i caratteri trovati con "*"

    cout << risultato << endl; // output: "C**o *m*c* pr*gr*mm*t*r*" 

    return 0;
}
```

Come abbiamo visto, le espressioni regolari possono essere utilizzate in una varietà di mod