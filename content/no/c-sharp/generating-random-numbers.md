---
title:                "Generering av tilfeldige tall"
html_title:           "C#: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?:

Å generere tilfeldige tall er en vanlig oppgave for mange programmerere. Dette refererer til produksjon av tall som ikke følger noen bestemte mønstre, og derfor er uforutsigbare. Programmerere bruker dette for en rekke formål, som å lage forskjellige spill, simulere data og utføre sikkerhetsrelaterte oppgaver.

Slik Gjør Du:

I C# kan du generere tilfeldige tall ved hjelp av Random-klassen. Først må du instansiere klassen ved å skrive ```C# Random random = new Random(); ``` i koden din. Deretter kan du bruke ulike metoder, som ```C# Next() ```, for å få tilfeldige tall. For eksempel, hvis du ønsker å generere et tilfeldig tall mellom 1 og 10, kan du skrive ```C# int number = random.Next(1, 11); ```, hvor 1 er det minste tallet og 11 ikke er inkludert, som gir deg tallene fra 1 til 10.

Dypdykk:

Den første metoden for tilfeldig tallgenerering ble utviklet av den britiske matematikeren George Everest. Moderne datamaskiner bruker en pseudorandom algoritme, som er en matematisk formel som produserer tall som er tilfeldig nok til å tilfredsstille de fleste behov. I tillegg til Random-klassen i C#, finnes det også andre alternativer for tilfeldig tallgenerering, som å bruke eksterne tjenester eller generere sanntidstall basert på eksterne variabler. Det er også viktig å være oppmerksom på at tilfeldige tall ofte ikke er helt tilfeldige, men kan følge et mønster som kan bli gjenkjent og utnyttet av hackere.

Se Også:

Du kan lære mer om tilfeldige tallgenerering i C# ved å følge denne lenken: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1. Det er også verdt å lese om tilfeldighet og sikkerhet i onlinekilder som denne: https://www.geeksforgeeks.org/how-randomized-algorithms-are-used-for-cryptography/.