---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
La concatenazione di stringhe è l'unione di due o più stringhe in una. I programmatori la usano per costruire messaggi di testo, formati di data e ora, query SQL, e più ancora.

## Come fare:
Inizia con l'operatore `+` per concatenare le stringhe:

```C#
string nome = "Mario";
string saluto = "Ciao, " + nome + "!";
Console.WriteLine(saluto);
// Output: Ciao, Mario!
```

Usa la funzione `String.Concat` per concatenare una lista di stringhe:

```C#
string[] parole = { "Ciao", "Mario,", "come", "stai?"};
string frase = String.Concat(parole);
Console.WriteLine(frase);
// Output: Ciao, Mario, come stai?
```

## Approfondimento:
La concatenazione delle stringhe è stata una funzionalità basilare della programmazione sin dalle sue prime fasi. Ma ricorda, il metodo `+` può essere costoso in termini di prestazioni se stai concatenando molte stringhe in un ciclo, causa la creazione di nuovi oggetti stringa ad ogni operazione. Alternativamente, puoi usare `StringBuilder` o `String.Join`.

`StringBuilder` è efficiente quando si esegue un numero elevato di operazioni di concatenazione:

```C#
var builder = new StringBuilder();
builder.Append("Ciao, ");
builder.Append("Mario");
Console.WriteLine(builder.ToString());
// Output: Ciao, Mario
```

`String.Join` è utile quando si dispone di una collezione di stringhe:

```C#
string[] parole = { "Ciao,", "Mario!" };
string frase = String.Join(" ", parole);
Console.WriteLine(frase);
// Output: Ciao, Mario!
```

## Vedi Anche:
* [String.Join Method](https://docs.microsoft.com/it-it/dotnet/api/system.string.join?view=netframework-4.8)
* [StringBuilder.Append Method](https://docs.microsoft.com/it-it/dotnet/api/system.text.stringbuilder.append?view=netframework-4.8)
* [String.Concat Method](https://docs.microsoft.com/it-it/dotnet/api/system.string.concat?view=netframework-4.8)