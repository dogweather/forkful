---
title:                "Eliminazione dei caratteri corrispondenti a un pattern."
html_title:           "C#: Eliminazione dei caratteri corrispondenti a un pattern."
simple_title:         "Eliminazione dei caratteri corrispondenti a un pattern."
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Se hai mai lavorato con stringhe di testo in C#, probabilmente sai quanto può essere frustrante dover rimuovere caratteri indesiderati da una stringa. È qui che entra in gioco la funzione di cancellazione di caratteri corrispondenti a un determinato pattern. Può semplificare significativamente il processo di manipolazione delle stringhe e risparmiare tempo e sforzi.

## Come fare

Per utilizzare la funzione di cancellazione di caratteri corrispondenti a un pattern in C#, per prima cosa dovrai identificare il pattern dei caratteri indesiderati che vuoi eliminare. Ad esempio, supponiamo che tu voglia rimuovere tutti i numeri dalla tua stringa. In questo caso, il pattern sarebbe [\d], dove [è un carattere speciale che indica che il carattere successivo è un numero. Dovrai utilizzare questo pattern all'interno del metodo Regex.Replace (), che sostituirà ogni carattere che corrisponde al pattern con una stringa vuota.

Per fare ciò, inizieremo creando una nuova soluzione C# in Visual Studio e ci aggiungeremo una nuova classe che chiameremo "Cancellazione pattern carattere.cs". All'interno della classe, creiamo un metodo statico che prende come parametro la stringa di input e restituisce la stringa di output dopo aver rimosso i caratteri corrispondenti al pattern.

```
public static string RimuoviCaratteriPattern(string input)
{
    Regex regex = new Regex(@"[\d]");
    string output = regex.Replace(input, "");
    return output;
}
```

Ora possiamo chiamare questo metodo all'interno del metodo Main () e passare una stringa di input per verificarne il funzionamento.

```
static void Main(string[] args)
{
    string input = "Questa_è 1_una 2_stringa_123_con 456_numeri.";
    string output = RimuoviCaratteriPattern(input);
    Console.WriteLine(output); // Output: Questa_è _una _stringa__con_numeri.
}
```

Come puoi vedere, tutti i numeri sono stati rimossi dalla stringa di input. È importante notare che la funzione di eliminazione caratteri corrispondenti al pattern è case-sensitive, quindi devi essere attento alla corrispondenza tra maiuscole e minuscole quando crei il tuo pattern.

## Deep Dive

Il metodo Regex.Replace () è estremamente potente e può essere utilizzato per manipolare le stringhe in molti modi diversi. Ad esempio, puoi utilizzare espressioni regolari più complesse per rimuovere caratteri di punteggiatura, spazi o anche parole dal tuo testo.

Inoltre, puoi anche utilizzare questo metodo per individuare e sostituire sottostringhe all'interno delle tue stringhe. Basta utilizzare un pattern diverso, ad esempio [a-zA-Z], per rilevare tutte le lettere dell'alfabeto.

È inoltre possibile utilizzare questo metodo all'interno di cicli o all'interno di metodi ricorsivi per eliminare continuamente i caratteri che corrispondono al pattern fino a quando la stringa finale desiderata viene raggiunta.

## Vedi anche

- [Documentazione Microsoft su Regex.Replace ()](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace)
- [Guida completa alle espressioni regolari in C#](https://www.c-sharpcorner.com/uploadfile/buradbramanand/regular-expressions-in-c-sharp/)
- [Primi passi con le espressioni regolari in C#](https://www.tutorialsteacher.com/csharp/csharp-regular-expression)