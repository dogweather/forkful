---
title:                "C#: Capitalizzazione di una stringa"
simple_title:         "Capitalizzazione di una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Una delle funzionalità fondamentali della programmazione è la capacità di manipolare e modificare i dati. Tra queste operazioni, il "capitalizzare" una stringa è un'azione comune che consente di convertire una parola o una frase inizialmente scritta in minuscolo in una versione con la prima lettera maiuscola.

## Come

Per eseguire questa operazione in C#, possiamo utilizzare il metodo `ToUpper` della classe `String`, che converte tutti i caratteri di una stringa in maiuscolo. Tuttavia, questo metodo non ci consente di capitalizzare solo la prima lettera della stringa. Per ottenerlo, possiamo combinare il metodo `ToUpper` con il metodo `Substring`:

```C#
string parolaIniziale = "casa";
string parolaCapitalizzata = parolaIniziale.ToUpper().Substring(0, 1) + parolaIniziale.Substring(1);

Console.WriteLine(parolaCapitalizzata); // Output: Casa
```

In questo esempio, abbiamo prima convertito la parola iniziale in maiuscolo con il metodo `ToUpper`, e poi abbiamo utilizzato il metodo `Substring` per prendere solo la prima lettera della stringa maiuscola e poi unire il resto della stringa originale. In questo modo otteniamo la nostra parola capitalizzata correttamente.

## Deep Dive

Se vogliamo essere più precisi e gestire anche i casi in cui la stringa iniziale potrebbe contenere già delle lettere maiuscole, possiamo utilizzare il metodo `Capitalize` della classe `TextInfo` presente nel namespace `System.Globalization`:

```C#
string parolaIniziale = "casa";
TextInfo textInfo = new CultureInfo("it-IT", false).TextInfo;
string parolaCapitalizzata = textInfo.ToTitleCase(parolaIniziale);

Console.WriteLine(parolaCapitalizzata); // Output: Casa
```

Utilizzando questo metodo, gestiamo correttamente anche i casi in cui la parola iniziale contiene già delle lettere maiuscole, ottenendo comunque un output corretto.

## Vedi anche

- [Método ToUpper (System.String)](https://docs.microsoft.com/dotnet/api/system.string.toupper)
- [Método Substring (System.String)](https://docs.microsoft.com/dotnet/api/system.string.substring)
- [Classe TextInfo (System.Globalization)](https://docs.microsoft.com/dotnet/api/system.globalization.textinfo)
- [Método ToTitleCase (System.Globalization.TextInfo)](https://docs.microsoft.com/dotnet/api/system.globalization.textinfo.totitlecase)