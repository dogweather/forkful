---
title:    "C#: Convertire una data in una stringa"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con date in un programma, è necessario mostrare la data in una stringa leggibile per gli utenti. Ad esempio, si può voler visualizzare la data di oggi in un formato comprensibile come "20 luglio 2021" invece che in un formato più tecnico come "07/20/2021". In questo caso, è necessario convertire la data in una stringa.

## Come fare

Per convertire una data in una stringa in C#, è possibile utilizzare il metodo `ToString()` dell'oggetto `DateTime`. Questo metodo accetta come argomento una stringa di formato che specifica la formattazione della data. Ad esempio, se si vuole visualizzare la data di oggi nel formato "dd MMMM yyyy", è possibile utilizzare il seguente codice:

```C#
DateTime oggi = DateTime.Today;
string dataStringa = oggi.ToString("dd MMMM yyyy");
Console.WriteLine(dataStringa);
```

L'output di questo codice sarebbe "20 luglio 2021", come desiderato.

Un'altra opzione è utilizzare il metodo `ToShortDateString()`, che converte la data in una stringa nel formato standard del sistema operativo. Ad esempio, se si utilizza un computer in italiano, l'output sarebbe "20/07/2021".

```C#
DateTime oggi = DateTime.Today;
string dataStringa = oggi.ToShortDateString();
Console.WriteLine(dataStringa);
```

## Approfondimento

Ci sono molti altri modi per formattare una data in una stringa in C#, come utilizzare i formati predefiniti di `DateTime` o creare un formato personalizzato specificando le opzioni di formattazione. Inoltre, è possibile utilizzare il metodo `ToString()` per formattare anche l'ora e il fuso orario della data.

È importante tenere conto del fatto che la formattazione della data dipende dalla cultura impostata nel sistema e che è possibile specificare una cultura diversa come argomento del metodo `ToString()` per ottenere un output corretto in base alle preferenze locali.

## Vedere anche

- [Documentazione ufficiale di Microsoft su `DateTime.ToString()`](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Esempi di formati di data e ora in C#](https://www.c-sharpcorner.com/article/date-and-time-format-in-c-sharp/)
- [Come cambiare la cultura della formattazione nella programmazione di C#](https://stackify.com/csharp-string-formatting/)