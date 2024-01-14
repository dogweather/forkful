---
title:    "C: Estrazione di sottostringhe"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Estrazione di sottostringhe può sembrare un compito insignificante, ma è un concetto fondamentale nella programmazione in C. Spesso ci troviamo a dover manipolare stringhe di dati, estraendo solo alcune parti di esse per ottenere il risultato desiderato. Conoscere come estrarre sottostringhe può semplificare notevolmente il nostro codice e renderlo più efficiente.

## Come fare

Per estrarre una sottostringa in C, dobbiamo utilizzare la funzione `strncpy()`, che ci consente di copiare solo un certo numero di caratteri da una stringa di origine in una destinazione. Ecco un esempio di come possiamo utilizzare questa funzione per estrarre una sottostringa di 3 caratteri:

```C
char source[] = "Pasta è deliziosa!";
char destination[4]; // la sottostringa è di 3 caratteri, ma abbiamo bisogno di 4 per il terminatore null

strncpy(destination, source + 6, 3); // copia 3 caratteri della stringa di origine a partire dal 6° carattere
destination[3] = '\0'; // aggiunge il terminatore null alla fine della sottostringa
printf("%s", destination); // output: è d
```

In questo esempio, abbiamo utilizzato una combinazione di `strncpy()` e l'operatore di scorrimento `+` per selezionare solo i caratteri che ci interessano dalla stringa di origine. Dopo aver copiato i caratteri nella destinazione, dobbiamo sempre ricordare di aggiungere il terminatore null alla fine della sottostringa per assicurarci che sia correttamente terminata.

## Approfondimento

Ma cosa succede effettivamente quando estraete una sottostringa? In realtà, quello che sta succedendo è che stiamo creando una nuova stringa, che ha le sue aree di memoria separate dalla stringa di origine. Questo significa che non stiamo modificando la stringa di origine, ma solo copiando alcune parti di essa in una nuova variabile. È importante tenere presente che le due stringhe sono completamente separate e una variazione nella sottostringa non influirà sulla stringa di origine.

Inoltre, è possibile utilizzare l'operazione di slicing utilizzata anche in altri linguaggi di programmazione, ad esempio `[inizio:fine:passo]`, per specificare ancora meglio quali caratteri estrarre dalla nostra stringa. Per esempio, se volessimo estrarre le ultime 4 lettere della stessa frase di prima:

```C
char source[] = "Pasta è deliziosa!";
char destination[5];

strncpy(destination, source + 13, 4);
destination[4] = '\0';
printf("%s", destination); // output: osa!
```

Come si può notare, questo ci ha permesso di selezionare solo gli ultimi caratteri della stringa di origine senza dover contare manualmente il numero di caratteri da copiare.

## Vedi anche

- `strncpy()` su [cplusplus.com](https://www.cplusplus.com/reference/cstring/strncpy/)
- Esempi di slicing in C su [GeeksforGeeks.org](https://www.geeksforgeeks.org/replace-part-string-another-given-string-c/)
- [Documentazione ufficiale di C](https://devdocs.io/c/) per approfondire ulteriormente l'argomento