---
title:    "Python: Å få gjeldende dato"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor

Når du utvikler programmer, kan det være nødvendig å få tak i den nåværende datoen. Dette kan være nyttig for en rekke grunner, som å spore når en bestemt hendelse skjedde, opprette datostempler for filer, eller som en del av en mer kompleks funksjon innenfor en større applikasjon. Uansett hva grunnen er, er det å få tak i den nåværende datoen en nyttig og viktig ferdighet for alle Python-utviklere.

## Hvordan

Det er flere måter å få tak i den nåværende datoen på i Python, men de to mest vanlige metodene er ved å bruke innebygde moduler eller funksjoner. Den innebygde "datetime" modulen i Python lar deg enkelt få tak i den nåværende datoen og klokkeslettet ved å bruke "datetime.now()" funksjonen. Dette vil returnere et "datetime" objekt som inneholder informasjon om den nåværende datoen og klokkeslettet.

```Python
import datetime

current_date = datetime.datetime.now() # Få tak i den nåværende datoen
print(current_date) # Skriv ut "datetime" objektet
```

Output:

`2021-01-27 12:30:00.573872`

En annen måte å få tak i den nåværende datoen på er gjennom "date" og "time" modulene, som lar deg tilpasse hvordan datoen og klokkeslettet blir presentert. For eksempel kan du bruke "date.today()" funksjonen for å få tak i dagens dato og deretter bruke "strftime()" funksjonen til å formatere datoen som du ønsker.

```Python
import datetime

current_date = datetime.date.today() # Få tak i dagens dato
print(current_date.strftime("%d/%m/%Y")) # Formatér datoen som dd/mm/yyyy
```

Output:

`27/01/2021`

## Dypdykk

Når du jobber med datoen i Python, er det viktig å forstå at "datetime" objekter kan utvides med flere funksjoner og metoder for å gjøre manipulasjoner og beregninger. For eksempel kan du bruke "timedelta" objektet til å legge til eller trekke fra dager, timer, minutter eller sekunder fra en dato. Dette kan være nyttig når du jobber med tidsintervaller eller behandler forskjellige tidssoner.

```Python
import datetime

current_date = datetime.datetime.now()
print(current_date + datetime.timedelta(days=7)) # Legg til 7 dager til den nåværende datoen
print(current_date - datetime.timedelta(hours=3)) # Trekk fra 3 timer fra den nåværende datoen
```

Output:

`2021-02-03 12:30:00.573872`

`2021-01-27 09:30:00.573872`

## Se også

- [Python datetime dokumentasjon](https://docs.python.org/3/library/datetime.html)
- [Python date og time dokumentasjon](https://docs.python.org/3/library/datetime.html#module-datetime)
- [Python timedelta dokumentasjon](https://docs.python.org/3/library/datetime.html#timedelta-objects)