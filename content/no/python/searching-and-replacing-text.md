---
title:    "Python: Søking og erstattelse av tekst"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en viktig del av programmering, spesielt når man jobber med store datasett eller tekstfiler. Det gjør det mulig å automatisere oppgaver og gjøre store mengder tekst endringer på en effektiv måte.

## Hvordan

For å søke og erstatte tekst i Python bruker vi funksjonen `replace()` som tar inn to argumenter, det gamle søkeordet og det nye ordet som skal erstatte det gamle. Vi kan også bruke `find()` funksjonen for å søke etter tekst og deretter bruke `replace()` for å erstatte den funnet teksten. La oss se et eksempel på kode:

```Python
tekst = "Dette er en teststring"
ny_tekst = tekst.replace("teststring", "øvingstekst")
print(ny_tekst)
```

Dette vil gi følgende output:

`"Dette er en øvingstekst"`

Vi kan også bruke `format()` funksjonen for å søke etter tekst og erstatte den med en variabel. La oss se på et eksempel:

```Python
navn = "Lisa"
tekst = "Hei, mitt navn er {}"
ny_tekst = tekst.format(navn)
print(ny_tekst)
```

Dette vil gi følgende output:

`"Hei, mitt navn er Lisa"`

## Dykk dypere

I tillegg til `replace()` funksjonen har Python også flere innebygde funksjoner for å søke og erstatte tekst, som for eksempel `re.sub()`. Denne funksjonen gir oss mer avanserte muligheter som å søke med regulære uttrykk.

Det er også verdt å merke seg at når vi jobber med store datasett eller tekstfiler, kan `replace()` funksjonen føre til problemer med minnebruk. For å unngå dette kan vi bruke `fileinput` modulen i Python.

## Se også

- [Python string documentation](https://docs.python.org/3/library/string.html)
- [Python regular expression documentation](https://docs.python.org/3/library/re.html)
- [Fileinput documentation](https://docs.python.org/3/library/fileinput.html)