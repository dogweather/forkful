---
title:    "Bash: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor

Noen ganger kan det være nyttig å kunne beregne en dato i fremtiden eller fortiden. Kanskje du planlegger en ferie og vil vite nøyaktig når du skal dra tilbake, eller kanskje du bare er nysgjerrig på hvilken dato det var for ti år siden. Uansett årsak, kan Bash-programmering hjelpe deg med å beregne datoer.

# Hvordan gjøre det

For å beregne en dato i Bash, må du først vite hvilken dato du vil starte fra. La oss si at vi vil beregne datoen to uker fra i dag. Vi kan gjøre dette ved å bruke kommandoen `date` i Bash.

```Bash
today=$(date +%s)
```

Her bruker vi `date` med argumentet `+%s` for å få datoen i sekunder siden 1. januar 1970. Dette nummeret representerer dagens dato og lagres i variabelen `today`.

Nå kan vi legge til to uker til dagens dato ved hjelp av `expr` kommandoen.

```Bash
future_date=$(expr $today + 1209600)
```

Siden vi vet at det er 60 sekunder i et minutt og 60 minutter i en time, multipliserer vi antallet uker (2) med antallet sekunder i en uke (604800) for å få den totale summen av sekunder som må legges til.

Til slutt kan vi bruke `date` kommandoen igjen for å konvertere det nye nummeret til en lesbar dato.

```Bash
future_date=$(date -d @$future_date)
echo "Datoen to uker fra nå vil være $future_date"
```

Resultatet vil være noe lignende "Datoen to uker fra nå vil være 28. juni 2019". For å beregne en dato i fortiden, kan du bruke `-` i stedet for `+` i `expr` kommandoen og velge et nummer mindre enn `today` for å trekke fra.

# Dypdykk

Det er mange muligheter for å bruke Bash-programmering til å beregne datoer. Du kan for eksempel bruke `while` løkker for å beregne en rekke datoer, eller bruke betingelser for å sjekke om datoen faller på en bestemt ukedag. Det finnes også en rekke tilgjengelige funksjoner og verktøy som gjør det enklere å håndtere datoer i Bash.

# Se også

- [Bash dokumentasjon om date kommandoen](https://www.gnu.org/software/bash/manual/html_node/Shell-Builtin-Commands.html)
- [Dato- og tidskalkulator i Bash script](https://tecadmin.net/calculate-date-time-bash-script/)
- [Video tutorial om å beregne datoer i Bash](https://www.youtube.com/watch?v=kbY5xnlvCxw)