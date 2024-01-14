---
title:                "C#: Concatenazione di stringhe"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare le stringhe è un'operazione fondamentale nella programmazione C# e può essere molto utile per unire diverse parti di un testo in un'unica stringa. 

## Come fare

Per concatenare le stringhe in C#, si possono utilizzare diversi approcci. Ecco un esempio di codice che utilizza l'operatore di concatenazione "+" per unire due stringhe:

```C#
string greeting = "Ciao";
string name = "Giuseppe";
string message = greeting + " " + name + "!"; 
Console.WriteLine(message);
```

Questo codice produrrà l'output "Ciao Giuseppe!".

Un altro modo per concatenare le stringhe è utilizzare il metodo `string.Concat()`, che prende in input un array di stringhe e le unisce insieme. Ecco un esempio:

```C#
string[] words = {"Questa", "è", "una", "frase"};
string sentence = string.Concat(words);
Console.WriteLine(sentence);
```

L'output di questo codice sarà "Questaèunafrase".

## Approfondimenti

La concatenazione di stringhe può essere molto utile quando si costruiscono query per database o quando si crea del testo dinamico da mostrare all'utente. Tuttavia, è importante tenere conto dell'efficienza del codice quando si utilizza l'operatore di concatenazione "+". Ogni volta che viene utilizzato, viene creato un nuovo oggetto `string`, il che può essere dispendioso in termini di risorse. 

Un modo per ottimizzare il codice è utilizzare la classe `StringBuilder` invece di concatenare direttamente le stringhe. `StringBuilder` è progettato specificamente per la manipolazione di stringhe e offre un'implementazione più efficiente per la concatenazione di varie stringhe. Ecco un esempio:

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Questo");
sb.Append("è");
sb.Append("un esempio");
sb.Append("di utilizzo");
sb.Append("della classe StringBuilder");
string sentence = sb.ToString();
Console.WriteLine(sentence);
```

L'output sarà "Questoèun esempio di utilizzodella classe StringBuilder". 

## Vedi anche
- [Documentazione ufficiale di Microsoft sulla classe `string`](https://docs.microsoft.com/it-it/dotnet/api/system.string?view=net-5.0)
- [Documentazione ufficiale di Microsoft sulla classe `StringBuilder`](https://docs.microsoft.com/it-it/dotnet/api/system.text.stringbuilder?view=net-5.0)