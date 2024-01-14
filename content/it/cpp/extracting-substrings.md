---
title:                "C++: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

##Perché

Molti programmatori si trovano spesso nella situazione in cui devono gestire grandi stringhe di testo e hanno bisogno di estrarre solo una parte specifica di essa. Questo è quando l'estrazione di sottostringhe diventa utile. Con poche linee di codice, è possibile ottenere solo la porzione desiderata della stringa, risparmiando tempo e semplificando il processo di programmazione.

##Come Fare

Per estrarre una sottostringa da una stringa più grande, è necessario utilizzare la funzione substr() in C++. Questa funzione prende due parametri: la posizione iniziale della sottostringa e la lunghezza della sottostringa da estrarre. Ad esempio, per estrarre la parola "ciao" dalla stringa "Buongiorno a tutti, ciao!", il codice sarebbe il seguente:

```C++
string testo = "Buongiorno a tutti, ciao!";
string sottostringa = testo.substr(19, 4); //19 è l'indice iniziale per "ciao" e 4 è la lunghezza della sottostringa
cout << sottostringa << endl; //stamperà "ciao"
```

Un'altra opzione è utilizzare la funzione find() per trovare la posizione di una parola specifica all'interno di una stringa, e quindi utilizzare la funzione substr() per estrarre la sottostringa. Ad esempio, se vogliamo estrarre la parola "ciao" dalla stringa di prima senza sapere l'indice, possiamo utilizzare il seguente codice:

```C++
string testo = "Buongiorno a tutti, ciao!";
int indice = testo.find("ciao"); //trova l'indice della prima occorrenza di "ciao"
string sottostringa = testo.substr(indice, 4); //utilizza il valore dell'indice per estrarre la sottostringa di 4 caratteri
cout << sottostringa << endl; //stamperà "ciao"
```

##Approfondimenti

È importante tenere conto della posizione degli indici quando si utilizza la funzione substr(). Gli indici delle stringhe iniziano da 0, quindi la prima parola in una stringa ha l'indice 0. Inoltre, la lunghezza della sottostringa deve essere minore o uguale alla lunghezza totale della stringa. Se la lunghezza specificata è maggiore della lunghezza della stringa, vengono estratti tutti i caratteri rimanenti fino alla fine della stringa.

##Vedi Anche

- [Documentazione su substr()](https://www.cplusplus.com/reference/string/string/substr/)
- [Documentazione su find()](https://www.cplusplus.com/reference/string/string/find/)