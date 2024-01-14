---
title:                "C#: Conversione di una data in una stringa"
simple_title:         "Conversione di una data in una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Spesso, quando si lavora con le date in un programma, c'è la necessità di convertirle in una stringa. Questo può essere utile per visualizzare la data in un formato specifico o per passarla in una richiesta API. In questo articolo, vedremo come convertire una data in una stringa utilizzando il linguaggio di programmazione C#.

## Come Fare
Per prima cosa, è necessario creare un oggetto Date dal quale si vuole estrarre una stringa. Supponiamo che vogliamo convertire la data odierna.
```
C# var today = DateTime.Today;
```

Successivamente, è possibile utilizzare il metodo `ToString()` per convertire la data in una stringa. In questo metodo, è possibile specificare il formato desiderato utilizzando le opzioni di formato come `d` per la data breve, `D` per la data lunga, `t` per l'ora breve e `T` per l'ora lunga.
```
C# var todayAsString = today.ToString("d");
Console.WriteLine(todayAsString);
Output: 12/12/2021
```

Se vogliamo aggiungere l'ora alla stringa, possiamo aggiungere il formato dell'ora al metodo `ToString()`.
```
C# var todayWithTimeAsString = today.ToString("d t");
Console.WriteLine(todayWithTimeAsString);
Output: 12/12/2021 11:30 AM
```

In alternativa, possiamo utilizzare il metodo `ToString()` senza specificare alcun formato per ottenere una stringa nel formato di data predefinito del computer.
```
C# var defaultFormatAsString = today.ToString();
Console.WriteLine(defaultFormatAsString);
Output: 12/12/2021 11:30:00 AM
```

## Approfondimento
Quando si utilizza il metodo `ToString()` per convertire una data in una stringa, è importante ricordare che il risultato dipenderà dalle impostazioni regionali del computer in cui il programma viene eseguito. Ad esempio, in Italia, il formato predefinito della data è giorno/mese/anno, mentre in altre parti del mondo potrebbe essere mese/giorno/anno. Questo potrebbe causare errori o problemi di formattazione se il programma viene eseguito su un computer con impostazioni regionali diverse. 

Inoltre, è possibile utilizzare il metodo `ToString()` con delle stringhe di formato personalizzate. Ad esempio, possiamo utilizzare `MMMM` per ottenere il nome completo del mese, `ddd` per ottenere il nome del giorno della settimana abbreviato o `yyyy` per ottenere l'anno completo.
```
C# var customFormat = today.ToString("dddd, dd MMMM yyyy");
Console.WriteLine(customFormat);
Output: domenica, 12 dicembre 2021
```

Conoscere le opzioni di formato disponibili e come utilizzarle correttamente può risultare molto utile quando si lavora con le date in C#. 

## Vedi Anche
- [Microsoft Docs: Metodo DateTime.ToString()](https://docs.microsoft.com/it-it/dotnet/api/system.datetime.tostring?view=net-6.0) 
- [Microsoft Docs: Formato della stringa Data e Ora personalizzato](https://docs.microsoft.com/it-it/dotnet/standard/base-types/custom-date-and-time-format-strings) 
- [Microsoft Docs: Proprietà CultureInfo.CurrentCulture](https://docs.microsoft.com/it-it/dotnet/api/system.globalization.cultureinfo.currentculture?view=net-6.0)