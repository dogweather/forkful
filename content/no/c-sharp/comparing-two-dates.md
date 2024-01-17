---
title:                "Sammenligning av to datoer"
html_title:           "C#: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Sammenligning av to datoer er en vanlig oppgave for programvareutviklere. Dette gjøres for å sammenligne tidsdata og identifisere forskjeller eller likheter mellom dem. Dette kan være nyttig for å håndtere dato- og klokkeslettbaserte funksjoner i applikasjoner, som for eksempel å beregne hvor lenge det har gått siden en hendelse eller å sortere data etter dato.

## Slik gjør du det:

For å sammenligne to datoer i C# kan du bruke metoden `Compare` i klassen `DateTime`. Denne metoden sammenligner to datoer og returnerer en hel verdi som indikerer om den første datoen er tidligere, senere eller lik den andre datoen. Her er et eksempel på hvordan du kan bruke denne metoden:

```C#
DateTime dato1 = new DateTime(2021, 3, 15);
DateTime dato2 = new DateTime(2021, 3, 20);

int resultat = DateTime.Compare(dato1, dato2);

// Resultatet vil være et negativt tall, et positivt tall eller 0 avhengig av datoene
```

Du kan også bruke `CompareTo`-metoden i stedet for `Compare`. Denne metoden fungerer på samme måte, men returnerer en boolean-verdi (`true` eller `false`) i stedet for en hel verdi.

## Dypdykk:

I eldre versjoner av C# måtte utviklere sammenligne datoer ved å kalle `ToUniversalTime()`-metoden på hver dato før de kunne sammenlignes. Dette skyldtes noen forskjeller i hvordan datoer ble lagret og behandlet internt i .NET Platform. Men i nyere versjoner, som C# 8.0, er dette problemet løst, og de nye metoden `Compare` og `CompareTo` tar hensyn til dette selv.

En alternativ måte å sammenligne datoer på er ved å bruke `Equals()`-metoden i stedet for `Compare`. Denne metoden sjekker kun om to datoer er like, og returnerer en boolean-verdi. Dette kan være nyttig hvis du ikke trenger å vite om en dato er tidligere eller senere enn en annen, kun om de er like.

## Se også:

For mer informasjon og eksempler på hvordan du kan sammenligne datoer i C#, kan du se Microsofts dokumentasjon [her](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=net-5.0) og [her](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compareto?view=net-5.0).