---
title:    "C#: Trova la lunghezza di una stringa"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Perché

Cercare la lunghezza di una stringa è un'operazione molto comune nella programmazione, in quanto ci permette di ottenere informazioni importanti sui dati che stiamo manipolando. Ad esempio, possiamo utilizzare la lunghezza di una stringa per controllare se è stata inserita una password abbastanza lunga o per verificare se un input utente ha superato un limite di caratteri.

## Come fare

Per trovare la lunghezza di una stringa in C#, possiamo utilizzare il metodo Length della classe string. Questo ci restituirà il numero di caratteri presenti nella stringa. Ecco un esempio di codice che mostra come usare questo metodo:

```C#
string nome = "Mario";
Console.WriteLine(nome.Length); // Output: 5
```

Come è possibile vedere dall'esempio, il valore restituito dal metodo Length corrisponde alla lunghezza della stringa "Mario", che contiene 5 caratteri.

## Approfondimento

Oltre al metodo Length, possiamo anche utilizzare la classe System.Text.StringBuilder per trovare la lunghezza di una stringa. Questa classe ha un metodo chiamato Capacity che ci permette di ottenere la capacità totale della stringa e un altro metodo chiamato Length che ci restituisce la lunghezza effettiva della stringa. Ecco un esempio di codice che mostra come utilizzare questi metodi:

```C#
StringBuilder sb = new StringBuilder("Questo è un esempio");
Console.WriteLine(sb.Length); // Output: 18
Console.WriteLine(sb.Capacity); // Output: 32
```

In questo caso, il metodo Capacity ci restituisce la capacità totale della stringa, che è maggiore della sua lunghezza effettiva. Questo perché la classe StringBuilder è progettata per gestire stringhe molto più lunghe di quelle standard.

## Vedi anche

- Documentazione ufficiale di Microsoft su come trovare la lunghezza di una stringa in C#: https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/strings/how-to-determine-the-length-of-a-string
- Stack Overflow: Qual è il modo più efficiente per trovare la lunghezza di una stringa in C#?: https://stackoverflow.com/questions/3395703/whats-the-most-efficient-way-to-get-the-length-of-a-string-in-c-sharp