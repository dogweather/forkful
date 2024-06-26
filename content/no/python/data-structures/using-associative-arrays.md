---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:47.661948-07:00
description: "Hvordan: \xC5 lage en ordbok i Python er enkelt. Du omslutter n\xF8\
  kkel-verdipar i kr\xF8llparenteser `{}`, med n\xF8kler og verdier skilt av et kolon."
lastmod: '2024-03-13T22:44:40.353934-06:00'
model: gpt-4-0125-preview
summary: "\xC5 lage en ordbok i Python er enkelt."
title: Bruke associative tabeller
weight: 15
---

## Hvordan:
Å lage en ordbok i Python er enkelt. Du omslutter nøkkel-verdipar i krøllparenteser `{}`, med nøkler og verdier skilt av et kolon:

```Python
# Lag en assosiativ tabell (ordbok)
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

Output:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

Å få tilgang til en verdi ved hjelp av nøkkelen er enkelt:

```Python
# Få tilgang til en verdi
print(my_dict["name"])
```

Output:
```
John
```

Å legge til eller oppdatere elementer gjøres ved å tilordne en verdi til en nøkkel:

```Python
# Legg til et nytt nøkkel-verdipar
my_dict["email"] = "john@example.com"
# Oppdater en verdi
my_dict["age"] = 31
print(my_dict)
```

Output:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

For å iterere over ordbokens elementer:

```Python
# Iterer gjennom nøkkel-verdipar
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

Output:
```
name: John
age: 31
city: New York
email: john@example.com
```

## Dypdykk
Assosiative tabeller i Python, eller ordbøker, ble introdusert for å tilby en datastruktur for effektiv tilgang og manipulering av data. I motsetning til sekvenser, som er indeksert med en rekke tall, er ordbøker indeksert med nøkler, som kan være av en hvilken som helst uforanderlig type. Dette designvalget gjør ordbøker ideelt egnet for raske oppslagstabeller hvor nøkler kartlegger til unike verdier.

Historisk sett har Python-ordbøker vært implementert ved hjelp av en hashtabell, noe som sikrer at gjennomsnittlig tidskompleksitet for oppslags-, innsettings- og sletteoperasjoner er O(1). I Python 3.6 og senere opprettholder ordbøker også innsettingsrekkefølgen av elementer, noe som kombinerer fordelene med hashtabeller med forutsigbarheten av innsettingsrekkefølge sett i ordnede datastrukturer.

Selv om ordbøker er utrolig allsidige, kan det i noen spesialiserte tilfeller være at alternativer som `collections.defaultdict` eller `collections.OrderedDict` (før Python 3.7) er å foretrekke. `defaultdict` er spesielt nyttig når du trenger en ordbok til å returnere en standardverdi for ikke-eksisterende nøkler, noe som forenkler visse typer betinget logikk. Men, med den kontinuerlige forbedringen og utviklingen av Python, forblir den innebygde ordboksklassen ofte det foretrukne valget for assosiative tabeller på grunn av dens robusthet og den bekvemmeligheten den tilbyr rett fra boksen.
