---
title:    "C#: Capitalizzazione di una stringa"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Perché capitalizzare una stringa in C#

La capitalizzazione di una stringa è un'operazione comune nella programmazione e consiste nel rendere maiuscola la prima lettera di una parola o di una frase. Questo può essere utile per presentare in modo più formale o leggibile i dati all'utente o per confrontare stringhe in modo non case-sensitive.

## Come fare

Ci sono diverse modalità per capitalizzare una stringa in C#. Una delle più semplici è utilizzare il metodo `ToUpper()` che trasforma tutti i caratteri in maiuscolo:

```C#
string name = "mario";
string capitalized = name.ToUpper();
Console.WriteLine(capitalized);
// output: MARIO
```

In alternativa, è possibile utilizzare il metodo `Substring()` per dividere la stringa in due parti e utilizzare il metodo `ToUpper()` solo sulla prima lettera:

```C#
string name = "mario";
string capitalized = name.Substring(0, 1).ToUpper() + name.Substring(1);
Console.WriteLine(capitalized);
// output: Mario
```

## Approfondimento

Mentre le due modalità sopra descritte funzionano per capitalizzare una sola parola, ci possono essere situazioni in cui è necessario capitalizzare l'intera frase. In questi casi, è possibile utilizzare il metodo `Split()` per dividere la stringa in parole e poi capitalizzare ogni parola usando uno dei due metodi visti in precedenza. Ad esempio:

```C#
string sentence = "ciao a tutti";
string[] words = sentence.Split(' ');
for(int i = 0; i < words.Length; i++){
    words[i] = words[i].Substring(0, 1).ToUpper() + words[i].Substring(1);
}
string capitalized = String.Join(" ", words);
Console.WriteLine(capitalized);
// output: Ciao A Tutti
```

Inoltre, esistono anche altri metodi più complessi che permettono di capitalizzare stringhe con regole specifiche, come ad esempio il metodo `ToTitleCase()` della classe `TextInfo`.

## Vedi anche

- [Documentazione ufficiale di C# sul metodo ToUpper()](https://docs.microsoft.com/it-it/dotnet/api/system.string.toupper)
- [Guida su come capitalizzare una stringa in C#](https://www.tutsmake.com/csharp/csharp-program-to-capitalize-every-word-in-string/)
- [Esempi di utilizzo del metodo ToTitleCase()](https://www.c-sharpcorner.com/UploadFile/855fc9/convert-string-to-title-case-in-C-Sharp/)