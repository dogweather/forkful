---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Eliminare i caratteri corrispondenti a un certo modello significa rimuovere dalla stringa tutti i caratteri che si adattano a un determinato schema, come una certa lettera o una sequenza di lettere. I programmatori fanno ciò per pulire i dati, rimuovere i caratteri non necessari o innecessari, o preparare la stringa per un'ulteriore elaborazione.

## Ecco come:
Eliminare i caratteri che corrispondono a un pattern in C# è molto semplice grazie alla funzione `Regex.Replace`:

```csharp
string testo = "C_aa#&a__a";
string pattern = "[^a-zA-Z0-9]"; // pattern per rimuovere tutto ciò che non è una lettera o un numero
string resultato = Regex.Replace(testo, pattern, "");
Console.WriteLine(resultato); // Ritornerà "Caaaa"
```
Quando si esegue questo codice, vengono rimossi tutti i caratteri che non sono lettere o numeri dalla stringa `testo`.

## Facciamo un tuffo più profondo:
L'approccio sopra menzionato ha le sue radici nelle espressioni regolari, un potente strumento introdotto negli anni '50. Le espressioni regolari sono un linguaggio standardizzato per identificare pattern nelle stringhe.

Si potrebbero considerare come alternativa l'uso dei metodi built-in di C# come `String.Replace` o `StringBuilder.Replace`, ma questi non offrono la stessa flessibilità nel gestire pattern complessi come le espressioni regolari.

Quando si utilizza `Regex.Replace`, viene creato un modello di espressione regolare, che viene poi utilizzato per scansionare la stringa di input. Ogni volta che viene trovato un match, viene rimosso. Questo avviene in modo iterativo su tutta la stringa.

## Per saperne di più:
- Documentazione Microsoft sulle espressioni regolari: [qui](https://docs.microsoft.com/it-it/dotnet/standard/base-types/regular-expressions)
- `Regex.Replace` nel dettaglio: [qui](https://docs.microsoft.com/it-it/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
- `String.Replace` e `StringBuilder.Replace`: [qui](https://docs.microsoft.com/it-it/dotnet/api/system.string.replace?view=net-5.0) e [qui](https://docs.microsoft.com/it-it/dotnet/api/system.text.stringbuilder.replace?view=net-5.0)