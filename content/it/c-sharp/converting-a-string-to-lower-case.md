---
title:                "C#: Trasformare una stringa in minuscolo"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler convertire una stringa in minuscolo utilizzando il linguaggio C#. Potrebbe essere necessario per confrontare stringhe in modo case-insensitive, per lavorare con database che non sono sensibili alle maiuscole e minuscole o semplicemente per una questione di preferenza personale.

## Come fare

Per convertire una stringa in minuscolo in C#, puoi utilizzare il metodo "ToLower()" della classe "string". Vediamo un esempio pratico:

```C#
string testo = "Questo è un TESTO in MAIUSCOLO";
string testoMinuscolo = testo.ToLower();

Console.WriteLine(testo); // Output: Questo è un TESTO in MAIUSCOLO 
Console.WriteLine(testoMinuscolo); // Output: questo è un testo in maiuscolo
```

Come puoi vedere, il nostro testo è stato correttamente convertito in minuscolo. Un'altra opzione è utilizzare il metodo "ToLowerInvariant()", che garantisce la stessa conversione in tutti i contesti culturali. Ecco un esempio:

```C#
string testo = "CIAO";
string testoMinuscolo = testo.ToLowerInvariant();

Console.WriteLine(testo); // Output: CIAO
Console.WriteLine(testoMinuscolo); // Output: ciao
```

## Approfondimento

Ora che sappiamo come convertire una stringa in minuscolo in C#, vediamo alcuni dettagli più tecnici. In generale, le operazioni di conversione del case dipendono dalla cultura specifica in cui viene eseguito il codice. Ad esempio, in una cultura turca la lettera "i" potrebbe essere convertita nel carattere "ı", mentre in una cultura inglese verrà convertita in "i". Per questo motivo, è importante specificare la cultura corretta nel codice, utilizzando il parametro "Culture" del metodo "ToLower()".

Inoltre, ricorda che la stringa originale non verrà modificata dalla conversione, ma verrà restituita una nuova stringa con il case corretto.

## Vedi anche

- Documentazione ufficiale di Microsoft sulla conversione case sensitive in C#: https://docs.microsoft.com/it-it/dotnet/api/system.string.tolower?view=net-5.0
- Un tutorial su come utilizzare stringhe in C#: https://www.c-sharpcorner.com/article/working-with-strings-in-c-sharp/
- Una spiegazione dettagliata sulle culture nel .NET framework: https://docs.microsoft.com/it-it/dotnet/standard/base-types/culture-specific-formats

La conversione di una stringa in minuscolo è un'operazione comune durante lo sviluppo di software in C#. Speriamo che questo articolo ti sia stato utile per comprendere meglio come farlo in modo efficace e corretto. Continua a seguire il nostro blog per altri utili tutorial e approfondimenti su C#.