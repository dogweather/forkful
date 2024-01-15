---
title:                "Maiuscolare una stringa"
html_title:           "C#: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
Capitalize una stringa è un'operazione comune in programmazione, soprattutto quando si manipolano dati inseriti dagli utenti. Esempi di stringhe capitalizzate includono nomi propri o titoli di testi.

## Come fare
Per capitalizzare una stringa in C#, è possibile utilizzare il metodo `ToUpper()` o `ToLower()` della classe `String`. Ad esempio:

```C#
string nome = "marco";
string nomeCapitalizzato = nome.ToUpper(); // nomeCapitalizzato diventa "MARCO"
```

```C#
string cognome = "Rossi";
string cognomeCapitalizzato = cognome.ToLower(); // cognomeCapitalizzato diventa "rossi"
```

Si può anche utilizzare il metodo `CultureInfo` per specificare la lingua/cultura con cui si vuole capitalizzare la stringa. Ad esempio:

```C#
string nomeCompleto = "giulia bianchi";
string nomeCompletoCapitalizzato = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(nomeCompleto); // nomeCompletoCapitalizzato diventa "Giulia Bianchi"
```

## Deep Dive
Vale la pena notare che quando si utilizza il metodo `ToUpper()` o `ToLower()` su una stringa, viene restituito un nuovo oggetto stringa e non viene modificato l'oggetto originale. Inoltre, questi metodi assumono che il testo sia già nella forma corretta per essere capitalizzato, quindi se la stringa contiene già lettere maiuscole o minuscole, queste verranno soltanto mantenute o cambiate, ma non verranno aggiunte o rimosse.

Tuttavia, utilizzando il metodo `CultureInfo` è possibile effettuare una capitalizzazione più accurata, poiché tiene conto delle regole linguistiche della cultura specificata. Ad esempio, se si utilizza il metodo `ToTitleCase` con la cultura inglese, gli articoli come "the" o le preposizioni come "of" non verranno capitalizzati all'inizio di una parola, mentre verranno capitalizzati se la cultura è francese o tedesca.

Inoltre, è possibile capitalizzare soltanto la prima lettera di ogni parola all'interno di una stringa utilizzando il metodo `ToTitleCase`, mentre i metodi `ToUpper()` e `ToLower()` capitalizzano o rendono tutto minuscolo l'intera stringa.

## Vedi anche
- [Documentazione di Microsoft su metodi `ToUpper()` e `ToLower()`](https://docs.microsoft.com/it-it/dotnet/api/system.string.toupper?view=netcore-3.1)
- [Documentazione di Microsoft su metodo `ToTitleCase()`](https://docs.microsoft.com/it-it/dotnet/api/system.globalization.textinfo.totitlecase?view=netcore-3.1)