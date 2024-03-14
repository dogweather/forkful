---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:06.852293-07:00
description: "Assosiative tabeller er som superladde tabeller som lar deg bruke strenger\
  \ som indekser i stedet for bare heltall. Programmerere bruker dem til mer\u2026"
lastmod: '2024-03-13T22:44:40.965275-06:00'
model: gpt-4-0125-preview
summary: "Assosiative tabeller er som superladde tabeller som lar deg bruke strenger\
  \ som indekser i stedet for bare heltall. Programmerere bruker dem til mer\u2026"
title: Bruke associative tabeller
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative tabeller er som superladde tabeller som lar deg bruke strenger som indekser i stedet for bare heltall. Programmerere bruker dem til mer komplekse datastrukturer, noe som gjør det enklere å håndtere data som ikke passer pent inn i en sekvensiell liste.

## Hvordan:

Først, erklær en assosiativ tabell i Bash:

```Bash
declare -A my_array
```

Deretter kan du begynne å fylle den med verdier, ved å bruke strenger som nøkler:

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Programmering"
```

For å få tilgang til et element, bruk nøkkelen:

```Bash
echo ${my_array["name"]}  # Utdata: Linux Journal
```

Å iterere over nøkler og verdier er også rett frem:

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

Eksempel på utdata kunne se slik ut:

```
name: Linux Journal
topic: Programmering
```

For å legge til eller endre elementer, bare tilordne en verdi til en nøkkel, på samme måte som den første fyllingen:

```Bash
my_array["readers"]="Du"
```

Og for å fjerne et element, bruk `unset`:

```Bash
unset my_array["topic"]
```

## Dypdykk

Assosiative tabeller ble introdusert i Bash versjon 4.0, noe som gjør dem til en relativt ny tillegg til språket. Før deres introduksjon var håndtering av arrayer med ikke-heltall-indekser tungvint, ofte krever omveier eller eksterne verktøy som `awk` eller `sed`.

Under panseret implementerer Bash assosiative tabeller ved hjelp av hashtabeller. Denne implementeringen gjør det mulig for effektiv nøkkeloppslag, som forblir ganske konstant uavhengig av array-størrelsen, en kritisk funksjon for ytelse i skriptutførelse.

Selv om assosiative tabeller i Bash bringer mye kraft og fleksibilitet til shell-skripting, kommer de med sitt eget sett med begrensninger, som å være noe klønete å arbeide med sammenlignet med tabeller i høyere nivå-språk som Python eller JavaScript. For komplekse datamanipulasjonsoppgaver kan det fortsatt være verdt å vurdere eksterne verktøy eller språk som er bedre egnet for jobben.

Men, for mange typiske skriptoppgaver, gir assosiative tabeller et verdifullt verktøy i Bash-programmererens verktøykasse, og muliggjør mer lesbare og vedlikeholdbare skript ved å tillate bruk av meningsfulle strengnøkler i stedet for numeriske indekser.
