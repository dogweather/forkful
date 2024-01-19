---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Paragonare Due Date in C#

## Cos'è e perché?

Paragonare due date è fondamentale in programmazione. Ci permette di gestire i flussi di lavoro basati sul tempo, i piani di scadenza, le priorità e molto altro.

## Come fare:

Qui di seguito è mostrato come si possono paragonare due date in C#.

```C#
DateTime data1 = new DateTime(2020, 12, 18);
DateTime data2 = new DateTime(2020, 12, 25);

if (data1 > data2)
{
    Console.WriteLine("La data1 è successiva alla data2");
}
else if (data1 < data2)
{
    Console.WriteLine("La data1 è precedente alla data2");
}
else
{
    Console.WriteLine("Le due date sono uguali");
}
```

Output:
```C#
"La data1 è precedente alla data2"
```

## Approfondimenti

Confrontare due date è un processo che è esistito sin da quando la programmazione è iniziata. In C#, l'overload dell'operatore permette di utilizzare direttamente gli operatori di confronto. Tuttavia, ci sono alternative, come l'uso di Compare e Equals metodi.

`DateTime.Compare` restituisce -1, 0 o 1 a seconda che la data di sinistra sia minore, uguale o maggiore di quella di destra.

```C#
int confronto = DateTime.Compare(data1, data2);
```

`DateTime.Equals` restituisce true se le due date sono identiche.
```C#
bool sonoUguali = DateTime.Equals(data1, data2);
```

## Vedi Anche

[Documentazione ufficiale Microsoft](https://docs.microsoft.com/it-it/dotnet/api/system.datetime.compare?view=net-5.0)

[DateTime in C# - Microsoft Learn](https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/inside-a-program/coding-conventions)

[C# DateTime Comparisons (Stack overflow)](https://stackoverflow.com/questions/271398/what-are-your-favorite-extension-methods-for-c-net-bonus-points-for-creativity)