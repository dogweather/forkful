---
title:                "C#: Capitalizzazione di una stringa"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
Prima di iniziare a parlare della capitalizzazione di una stringa in C#, è importante comprendere perché questo argomento è importante per i programmatori. Quando si lavora con stringhe, a volte è necessario che determinati caratteri siano in maiuscolo o minuscolo per avere una corretta formattazione o per confrontare le stringhe. La capitalizzazione di una stringa ci permette di manipolarla per soddisfare le nostre esigenze durante la programmazione.

## Come Fare
Per capitalizzare una stringa in C#, possiamo utilizzare il metodo ToUpper() presente nella classe string. Vediamo un esempio di codice che mostra come utilizzarlo:

```C#
string stringa = "ciao a tutti";
string stringaCapitalizzata = stringa.ToUpper();
Console.WriteLine(stringaCapitalizzata);
```

Output: CIAO A TUTTI

In questo esempio, abbiamo dichiarato una variabile di tipo stringa e l'abbiamo inizializzata con il valore "ciao a tutti". Utilizzando il metodo ToUpper(), abbiamo assegnato il valore capitalizzato alla nuova variabile stringaCapitalizzata e l'abbiamo stampata a schermo.

## Approfondimento
Mentre il metodo ToUpper() è utile per capitalizzare tutte le lettere di una stringa, è importante notare che non fa solo la conversione delle lettere dall'alfabeto inglese, ma funziona anche con caratteri speciali, come ad esempio l'accento. Vediamo un esempio di codice:

```C#
string stringa = "àbÇdèFgÌ";
string stringaCapitalizzata = stringa.ToUpper();
Console.WriteLine(stringaCapitalizzata);
```

Output: ÀBÇDÈFGÌ

Come vediamo, il metodo ha capitalizzato tutte le lettere, incluso l'accento sulla lettera a e la lettera i maiuscola con l'accento grave.

## Vedi Anche
Ecco alcune risorse utili per imparare di più sulla capitalizzazione di stringhe in C#:

- [Documentazione ufficiale Microsoft su ToUpper()](https://docs.microsoft.com/it-it/dotnet/api/system.string.toupper?view=net-5.0)
- [Tutorial su come manipolare stringhe in C#](https://www.tutorialspoint.com/csharp/csharp_strings.htm)
- [Guida pratica per la programmazione in C#](https://www.c-sharpcorner.com/UploadFile/mgold/IntroductionToCSharp11032005003758AM/IntroductionToCSharp.aspx)

Grazie per aver letto questo articolo e buon divertimento con la capitalizzazione di stringhe in C#!