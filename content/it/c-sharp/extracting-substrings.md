---
title:                "Estrarre sottostringhe"
html_title:           "C#: Estrarre sottostringhe"
simple_title:         "Estrarre sottostringhe"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
Se stai lavorando su un progetto di programmazione in C#, potresti aver bisogno di estrarre delle sottostringhe da una stringa più grande. Questo può essere utile per manipolare i dati o per ottenere informazioni specifiche da un testo più grande.

## Come
Estrarre una sottostringa in C# è abbastanza semplice, grazie ai metodi integrati nella classe String. Ad esempio, se vogliamo estrarre una sottostringa che inizia al terzo carattere e finisce al quinto, possiamo utilizzare il metodo `Substring` in questo modo:

```C#
string testo = "Questo è un esempio di stringa.";
string sottostringa = testo.Substring(3, 3);

Console.WriteLine(sottostringa);
```

Questo dovrebbe stampare `sto`, poiché la sottostringa è composta dai caratteri dal terzo al quinto (escluso). È importante notare che il primo parametro del metodo `Substring` è l'indice del carattere a partire dal quale iniziare l'estrazione, mentre il secondo parametro indica il numero di caratteri da estrarre.

Possiamo anche utilizzare il metodo `Substring` per estrarre una sottostringa a partire da un certo carattere fino alla fine della stringa, semplicemente passando un solo parametro:

```C#
string testo = "Questa è un'altra stringa di esempio.";
string sottostringa = testo.Substring(6);

Console.WriteLine(sottostringa);
```

Questo dovrebbe stampare `è un'altra stringa di esempio.`, poiché il metodo `Substring` inizia l'estrazione dal sesto carattere (escluso) fino alla fine della stringa.

## Deep Dive
Oltre ai semplici casi di utilizzo descritti nella sezione precedente, esistono altri metodi per estrarre sottostringhe in C#. Ad esempio, il metodo `Split` può essere utilizzato per dividere una stringa in sottostringhe basate su un determinato carattere di separazione. Vediamo un esempio:

```C#
string testo = "Questo è un esempio di stringa.";
string[] sottostringhe = testo.Split(' ');

foreach (string s in sottostringhe)
{
    Console.WriteLine(s);
}
```

Questo dovrebbe stampare ogni parola della stringa precedente su una nuova riga, poiché stiamo dividendo la stringa utilizzando lo spazio come carattere di separazione.

Inoltre, possiamo anche utilizzare espressioni regolari per estrarre sottostringhe più complesse, ad esempio basate su un determinato pattern. Anche in questo caso, il C# offre dei metodi utili nella classe Regex per questo scopo.

## See Also
- Metodo `Substring` - documentazione ufficiale di Microsoft: https://docs.microsoft.com/it-it/dotnet/api/system.string.substring
- Metodo `Split` - documentazione ufficiale di Microsoft: https://docs.microsoft.com/it-it/dotnet/api/system.string.split
- Classe Regex - documentazione ufficiale di Microsoft: https://docs.microsoft.com/it-it/dotnet/api/system.text.regularexpressions.regex