---
title:                "Arduino: Å beregne en dato i fremtid eller fortid"
simple_title:         "Å beregne en dato i fremtid eller fortid"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne beregne en dato i fremtiden eller fortiden kan være nyttig i mange ulike situasjoner. Det kan være for å planlegge en reise, holde oversikt over fremtidige avtaler eller for å lage en tidslinje for et prosjekt. I denne bloggposten vil vi vise deg hvordan du kan bruke Arduino til å beregne en dato i fremtiden eller fortiden.

## Hvordan

For å kunne beregne en dato i fremtiden eller fortiden trenger vi å kjenne til dagens dato. Arduino har en innebygd funksjon som gjør dette, kalt ```now()```. Denne funksjonen returnerer dagens dato i form av et ```DateTime``` objekt. Vi kan deretter bruke dette objektet til å utføre beregningene våre.

For å beregne en dato i fremtiden, må vi først legge til et bestemt antall dager til dagens dato. Dette kan gjøres ved å bruke funksjonen ```addDays(days)``` på ```DateTime``` objektet vårt. Det totale antall dager legges deretter til i en variabel, for eksempel ```fremtidigDato```. Vi kan deretter bruke ```fremtidigDato``` til å skrive ut den beregnede datoen, for eksempel ved hjelp av funksjonen ```getDateStr()```.

```Arduino
DateTime nå = now();
DateTime fremtidigDato = nå.addDays(30);
Serial.println(fremtidigDato.getDateStr());
```

Dette vil skrive ut datoen 30 dager fra dagens dato. For å beregne en dato i fortiden, bruker vi samme prinsipp, bare med funksjonen ```subDays(days)``` i stedet.

```Arduino
DateTime nå = now();
DateTime fortidigDato = nå.subDays(15);
Serial.println(fortidigDato.getDateStr());
```

Dette vil skrive ut datoen 15 dager tilbake fra dagens dato. Ved å endre på antall dager som legges til eller trekkes fra, kan vi beregne en dato i hvilken som helst retning.

## Deep Dive

Det er også mulig å bruke Arduino til å beregne en dato med et spesifikt format. Dette kan være nyttig for å tilpasse formatet på den printede datoen til dine behov. For å gjøre dette, bruker vi funksjonen ```format(str)``` på ```DateTime``` objektet vårt. Inne i parentesen gir vi funksjonen et spesifikt format, for eksempel "yyyy-mm-dd hh:mm:ss".

```Arduino
DateTime nå = now();
DateTime fremtidigDato = nå.addDays(30);
Serial.println(fremtidigDato.format("yyyy-mm-dd"));
```

Dette vil skrive ut datoen 30 dager frem i et format som ligner på dette: "2021-11-05".

Det er også mulig å beregne en dato ved hjelp av andre enheter enn dager. For eksempel kan vi legge til timer, minutter eller sekunder til et ```DateTime``` objekt ved å bruke funksjonene ```addHours()```, ```addMinutes()``` eller ```addSeconds()```.

## Se også

- [DateTime dokumentasjon](https://www.arduino.cc/en/Reference/DateTime)
- [Tutorial: Using Time Library](https://www.arduino.cc/en/tutorial/time)
- [Tutorial: Time and Date Basics](https://www.arduino.cc/en/Tutorial/TimeRTC)