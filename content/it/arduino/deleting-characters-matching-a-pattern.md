---
title:                "Arduino: Cancellare caratteri corrispondenti a un pattern"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ciao a tutti, oggi parleremo di come eliminare i caratteri corrispondenti a un determinato schema utilizzando Arduino. Potresti trovarti in questa situazione se stai lavorando su un progetto che richiede l'analisi e la manipolazione delle stringhe di testo. In questo articolo, imparerai il motivo per cui è importante conoscere questo concetto e come farlo utilizzando il linguaggio di programmazione Arduino.

## Come Fare

Per prima cosa, è necessario capire cosa si intende per "caratteri matching a pattern". In parole semplici, è quando si cerca una determinata sequenza di caratteri all'interno di una stringa di testo. Per farlo in Arduino, abbiamo a disposizione la funzione "if", che ci permette di eseguire una determinata azione solo se la condizione specificata è vera.

Per eliminare i caratteri corrispondenti a un determinato schema, dobbiamo utilizzare una combinazione di cicli, condizioni e funzioni stringa. Di seguito è riportato un esempio di codice che elimina tutti i caratteri "a" da una stringa e stampa il risultato:

```Arduino
String inputString = "Arduino è fantastico!";
String outputString;

//ciclo for per scorrere la stringa di input
for (int i = 0; i < inputString.length(); i++) {
	//condizione per verificare se il carattere corrente è diverso da "a"
	if (inputString.charAt(i) != 'a') {
		//aggiungere il carattere al risultato
		outputString += inputString.charAt(i);
	}
}

//stampare il risultato 
Serial.println(outputString); //stamperà "rduno è fntstico!"
```

Come puoi vedere, abbiamo utilizzato la funzione "charAt()" per ottenere il carattere corrente della stringa e una condizione "if" per verificare se corrisponde al carattere che vogliamo eliminare. Se non è così, lo aggiungiamo alla stringa di output utilizzando l'operatore "+=" che concatena i due caratteri.

Questo è solo un esempio di base e puoi modificarlo e adattarlo in base alle tue esigenze. Puoi anche utilizzare altre funzioni stringa come "substring()" o "indexOf()" per maggiori opzioni di manipolazione delle stringhe.

## Deep Dive

Ora che sai come eliminare i caratteri matching a un certo pattern, puoi utilizzare questa conoscenza per molteplici scopi. Ad esempio, potresti utilizzare questa tecnica per filtrare informazioni in un database o per analizzare dati ricevuti dai sensori.

Inoltre, è possibile implementare questo concetto in combinazione con altri comandi come la sostituzione di caratteri o la ricerca di pattern più complessi all'interno di una stringa. Ci sono molte possibilità e dipende tutto dalla tua immaginazione e dalle tue esigenze.

## See Also

Per maggiori informazioni su come manipolare le stringhe in Arduino, puoi consultare i seguenti link:

- [Funzioni String in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Tutorial su Stringhe e Char in Arduino](https://www.arduino.cc/en/Tutorial/StringCharacters)
- [Tutorial su Arrays, Stringhe e Char in Arduino](https://www.arduino.cc/en/Tutorial/Arrays)