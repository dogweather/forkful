---
title:                "C#: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Il processo di estrazione di sottostringhe è un'importante abilità da avere nella programmazione C#. Questa operazione permette di estrarre una porzione specifica di una stringa più grande, rendendo più facile manipolarla e utilizzarla in modo efficace.

## Come

Estrarre sottostringhe è un'operazione semplice, ma cruciale. Utilizzando il metodo `Substring()` in C#, è possibile specificare l'indice di inizio e la lunghezza della sottostringa desiderata. Ad esempio:

```C#
string myString = "Questo è un esempio di stringa.";
string subString = myString.Substring(10, 7);
Console.WriteLine(subString);
```

Questo codice produrrà un output di `esempio`, in quanto stiamo estraendo una sottostringa a partire dall'indice 10 per una lunghezza di 7 caratteri. 

Ci sono anche altre opzioni per estrarre sottostringhe, ad esempio utilizzando il metodo `Split()` per suddividere una stringa in base a un carattere specifico. Ad esempio:

```C#
string myString = "Questo è un esempio di stringa.";
string[] words = myString.Split(' ');
Console.WriteLine(words[3]);
```

Questo codice produrrà un output di `esempio`, in quanto stiamo dividendo la stringa in base agli spazi e selezionando la quarta parola.

## Deep Dive

Estrarre sottostringhe può essere ancora più utile quando si utilizzano espressioni regolari. Invece di specificare un indice di inizio e la lunghezza, è possibile definire un modello di ricerca per la sottostringa desiderata. Ad esempio:

```C#
string myString = "Il mio numero di telefono è 555-123-4567.";
string pattern = @"\d{3}-\d{3}-\d{4}";
Match match = Regex.Match(myString, pattern);
Console.WriteLine(match.Value);
```

Questo codice produrrà un output di `555-123-4567`, in quanto stiamo cercando un numero di telefono nel formato specificato dall'espressione regolare.

## Vedi anche

- [Documentazione Microsoft su `Substring()`](https://docs.microsoft.com/it-it/dotnet/api/system.string.substring)
- [Tutorial su espressioni regolari in C#](https://www.sololearn.com/Play/CSharp)