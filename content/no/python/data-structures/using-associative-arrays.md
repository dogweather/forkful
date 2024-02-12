---
title:                "Bruke associative tabeller"
aliases: - /no/python/using-associative-arrays.md
date:                  2024-01-30T19:12:47.661948-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke associative tabeller"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative tabeller, kjent i Python som ordbøker, kartlegger nøkler til verdier, noe som gjør det enkelt å hente, endre eller spore data via en unik identifikator. Programmerere bruker dem for deres effektivitet i å få tilgang til elementer og deres fleksibilitet i å representere komplekse datastrukturer.

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
