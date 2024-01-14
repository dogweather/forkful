---
title:    "C#: Utilizzo delle espressioni regolari"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore C# e stai cercando un modo per gestire e manipolare stringhe in modo più efficiente, le espressioni regolari sono lo strumento che fa per te. Con l'uso delle espressioni regolari puoi trovare, sostituire e controllare stringhe con una maggiore precisione e flessibilità rispetto ai metodi tradizionali.

## Come Utilizzarle

Le espressioni regolari sono supportate dal linguaggio C# tramite la classe Regex. Per utilizzarle, è necessario prima creare un'istanza di Regex con il pattern desiderato:

```
Regex regex = new Regex(@"([A-Za-z]+)\d+");
```

Nell'esempio sopra, stiamo cercando parole che iniziano con una lettera e sono seguite da uno o più numeri. Una volta creato un'istanza di Regex, possiamo utilizzarla per eseguire operazioni come il match e la sostituzione:

```
string input = "Abc123 Def456 Ghi789";
MatchCollection matches = regex.Matches(input);

foreach (Match match in matches)
{
    Console.WriteLine(match.Value);
}

string output = regex.Replace(input, "[$1-$1-$2]");
Console.WriteLine(output);
```

L'output per il codice sopra sarà:

```
Abc123
Def456
[Abc-Abc-123] [Def-Def-456] [Ghi-Ghi-789]
```

## Approfondimento

Le espressioni regolari possono sembrare spaventose e complicate, ma una volta che le si capisce bene, possono essere uno strumento molto potente per la manipolazione delle stringhe. Ci sono molti metodi e costrutti diversi che possono essere utilizzati nelle espressioni regolari, quindi per sfruttarle al massimo è importante familiarizzare con essi.

I link seguenti offrono tutorial e documentazione utili per imparare di più sulle espressioni regolari in C#:

- [Documentazione Microsoft su Regex](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Tutorial su Regex in C#](https://www.dotnetperls.com/regex)
- [Guida approfondita su Regex in C#](https://www.codeproject.com/articles/9099/the-30-minute-regex-tutorial)

## Vedi Anche

- [Guida all'utilizzo di Regex in C#](https://www.silverlight.it/progetti/regexhelper/)
- [Regex Tester e Cheat Sheet](https://regex101.com/)