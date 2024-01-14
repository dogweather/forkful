---
title:    "Bash: Ekstrahering av delstrenger"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor
Velkommen til min blogg som handler om å trekke ut substringer i Bash programmering! Hvis du er ny: substringer er en del av en tekst streng. Hvis du er en mer erfaren Bash-programmerer, vet du at substringer er en nyttig og nødvendig del av å håndtere tekstdata. Enten du er helt ny eller en erfaren programmerer, vil denne bloggposten gi deg verdifull informasjon om hvordan å ekstrahere substringer i Bash og hvorfor det er viktig.

## Hvordan
For å trekke ut substringer i Bash-programmering, må du bruke "```${STRING:START:LENGTH}```" syntaks. La oss si at vi har en variabel kalt "navn" som inneholder verdien "John Doe". For å hente ut substring "John", kan du bruke følgende kommando:
```Bash
navn="John Doe"
echo ${navn:0:4}     # Output: John
```

Hvis du vil hente ut siste del av navnet "Doe", kan du bruke følgende kommando:
```Bash
echo ${navn:5}       # Output: Doe
```

Du kan også bruke variabler som start og lengde når du ekstraherer substringer. La oss si at vi har en variabel "start" som er satt til 3 og "lengde" som er satt til 5. Da kan vi hente ut "n Doe" fra variable "navn" ved hjelp av følgende kommando:
```Bash
echo ${navn:$start:$lengde}      # Output: n Doe
```

Det er også mulig å hente ut substring fra slutten av en variabel ved hjelp av negative verdier for start og/eller lengde. La oss si at vi vil hente ut "Doe" fra variable "navn". Da kan vi bruke følgende kommando:
```Bash
echo ${navn:(-3)}      # Output: Doe
```

## Deep Dive
Som vi har sett i eksemplene over, kan vi hente ut forskjellige deler av en tekststreng ved hjelp av "substring"-syntaksen. Men det er også mulig å bruke "substring"-syntaksen på arrayer i Bash. Dette lar oss ekstrahere deler av et element i arrayet. La oss si at vi har et array med navn "array" som inneholder "Navn: John". Hvis vi vil hente ut "John" fra dette arrayet, kan vi bruke følgende kommando:
```Bash
echo ${array[1]:6}  # Output: John
```

Vi kan også bruke "substring"-syntaksen til å utføre utvidet regulær uttrykkssamsvar, også kalt "regex". Dette åpner for mer avansert substringsøking og utvinning. Til dette bruker vi "~" (tilde-operator) på følgende måte: 
```Bash
echo ${navn:~[0-9]}    # Output: Doe
```
Dette vil returnere alt etter det første sifferet i startposisjonen. Du kan bruke dette til å utvide dine Bash-programmeringsferdigheter ytterligere og automatisere forskjellige oppgaver.

## Se også
For mer informasjon om å trekke ut substringer i Bash-programmering, kan du se følgende ressurser:

- [GNU Bash manual om "substring"-syntaksen](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Bash Hackers Wiki om regulære uttrykk](http://wiki.bash-hackers.org/syntax/pattern)
- [Bash-håndboken av O-Gi](https://github.com/giampaolo/psutil/blob/master/BASHFAQ#L30-L35)

Takk for at du leste min bloggpost og lykke til med å trekke ut substringer i dine Bash-programmer!