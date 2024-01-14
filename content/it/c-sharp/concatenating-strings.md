---
title:    "C#: Concatenazione di stringhe"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è un'operazione fondamentale nell'ambito della programmazione, che consente di unire due o più stringhe per formare una nuova stringa. Questo può essere utile in molteplici situazioni, ad esempio per la creazione di messaggi personalizzati o la costruzione di URL dinamici.

## Come fare

Per concatenare stringhe in C#, è possibile utilizzare l'operatore "+" o il metodo string.Concat(). Vediamo un esempio di entrambi i metodi:

```C#
// Utilizzo dell'operatore "+"
string nome = "Marco";
string saluto = "Ciao " + nome;
Console.WriteLine(saluto);  // Output: Ciao Marco

// Utilizzo del metodo string.Concat()
string cognome = "Rossi";
string nomeCompleto = string.Concat(nome, " ", cognome);
Console.WriteLine(nomeCompleto);  // Output: Marco Rossi
```

Come si può notare, entrambi i metodi producono lo stesso risultato, ma l'utilizzo del metodo string.Concat() può essere più efficiente in caso di concatenazione di più stringhe.

È inoltre possibile concatenare stringhe con valori di altre tipologie di dati, grazie al metodo string.Format(). Vediamo un esempio:

```C#
string colorePreferito = "blu";
int numeroPreferito = 7;
string frase = string.Format("Il mio colore preferito è il {0} e il mio numero fortunato è il {1}.", colorePreferito, numeroPreferito);
Console.Write(frase);  // Output: Il mio colore preferito è il blu e il mio numero fortunato è il 7.
```

## Approfondimento

Di base, le stringhe in C# sono immutabili, ovvero non possono essere modificate direttamente. Quando si esegue una concatenazione di stringhe, in realtà si sta creando una nuova stringa che contiene il risultato della concatenazione, mentre le stringhe originali rimangono invariate. Questo può avere un impatto sulle prestazioni, soprattutto quando si lavora con un grande numero di stringhe o con cicli ripetuti di concatenazione.

Per evitare questo problema, è possibile utilizzare la classe StringBuilder, che permette di modificare direttamente una stringa senza creare ogni volta una nuova istanza. Vediamo un esempio di utilizzo:

```C#
StringBuilder nomeCompleto = new StringBuilder("Marco");
nomeCompleto.Append(" Rossi"); // Aggiunta del cognome al nome
Console.WriteLine(nomeCompleto);  // Output: Marco Rossi
```

Inoltre, esistono diverse funzioni di formattazione della stringa che possono essere utili per rendere più flessibile e leggibile il processo di concatenazione, come ad esempio il metodo string.Join() per unire elementi di un array di stringhe.

## Vedi anche

- Documentazione ufficiale di Microsoft su concatenare stringhe in C#: https://docs.microsoft.com/it-it/dotnet/csharp/how-to/concatenate-multiple-strings
- Tutorial YouTube su stringhe e concatenazione in C#: https://youtu.be/oiHcb6Xf16Y
- Esempi pratici di concatenazione di stringhe in un progetto C#: https://www.geeksforgeeks.org/concatenation-strings-c-sharp/