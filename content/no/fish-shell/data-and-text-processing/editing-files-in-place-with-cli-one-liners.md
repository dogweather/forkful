---
date: 2024-01-27 16:21:06.558328-07:00
description: "\xC5 redigere filer p\xE5 stedet med kommandolinje-enlinjere handler\
  \ om \xE5 gj\xF8re endringer direkte i filer fra kommandolinjen, uten \xE5 \xE5\
  pne dem i en\u2026"
lastmod: '2024-03-13T22:44:41.222824-06:00'
model: gpt-4-0125-preview
summary: "\xC5 redigere filer p\xE5 stedet med kommandolinje-enlinjere handler om\
  \ \xE5 gj\xF8re endringer direkte i filer fra kommandolinjen, uten \xE5 \xE5pne\
  \ dem i en tekstredigerer."
title: "Redigering av filer p\xE5 stedet med CLI-enlinjerskommandoer"
weight: 32
---

## Hvordan:
Fish Shell, kjent for sine brukervennlige funksjoner og kraftfulle skriptingsmuligheter, tilbyr flere måter å redigere filer på stedet på. Men, i motsetning til noen andre shells, har Fish ikke en innebygd mekanisme for redigering på stedet (`sed -i` i Bash, for eksempel). Men frykt ikke, du kan fortsatt oppnå dette med litt kreativitet og litt hjelp fra eksterne verktøy som `sed` og `awk`.

### Bruke `sed` for enkle erstatninger
For å erstatte alle forekomster av "hello" med "world" i `file.txt`, ville du bruke:
```Fish Shell
sed -i '' 's/hello/world/g' file.txt
```

### Påføre flere `sed`-kommandoer
Hvis du trenger å utføre flere erstatninger, kan du kjede dem slik:
```Fish Shell
sed -i '' -e 's/fish/bass/g' -e 's/rainbow/trout/g' file.txt
```

### Bruke `awk` for mer komplekse operasjoner
For operasjoner som er for komplekse for `sed`, kan `awk` være verktøyet du velger. Slik dobler du nummeret på hver linje:
```Fish Shell
awk '{print $1 * 2}' file.txt > temp && mv temp file.txt
```

### Merk om feilhåndtering
Husk at når du bruker disse verktøyene fra Fish, er det avgjørende å fange opp feil og forstå deres meldinger. Bruk Fish sin robuste feilhåndtering for å gjøre skriptene dine mer pålitelige.

## Dypdykk
Historisk sett har redigering av filer på stedet vært en grunnpilar i Unix og Linux-programmering, som tilbyr en effektiv måte å utføre raske redigeringer uten å manuelt åpne filer. Verktøy som `sed` og `awk` er ærverdige verktøy som har vært rundt siden de tidlige dagene av Unix og har blitt uunnværlige for tekstbehandlingsoppgaver.

Fish Shell, som er mer moderne og skryter av forbedringer i brukervennlighet og skripting, mangler innebygd redigering på stedet hovedsakelig på grunn av sin designtankegang fokusert på interaktivitet og brukervennlighet. Mangelen på en innfødt kommando for redigering på stedet i Fish understreker viktigheten av eksterne verktøyer i Unix-lignende økosystemer.

Alternativene for redigering på stedet i Fish inkluderer bruk av midlertidige filer eller å dra nytte av Perl eller Python enlinjere, som kan tilby mer fleksibilitet eller lesbarhet for komplekse oppgaver.

For eksempel ved bruk av Perl:
```Fish Shell
perl -pi -e 's/find/replace/g' file.txt
```
Eller Python:
```Fish Shell
python -c "import re, sys; [sys.stdout.write(re.sub('pattern', 'replacement', line)) for line in sys.stdin]" < file.txt > temp && mv temp file.txt
```

Når det gjelder implementering, når du utfører redigering på stedet, oppretter disse verktøyene vanligvis en midlertidig fil, skriver endringene der, og erstatter deretter den opprinnelige filen med den modifiserte versjonen. Denne tilnærmingen sikrer at filredigeringsprosessen ikke korrumperer eller mister data hvis det oppstår en feil under operasjonen.

Å forstå disse verktøyene og metodene gjør det mulig for Fish Shell-programmerere å effektivt inkorporere redigering på stedet i skriptene sine, og bygger bro mellom Fish sine brukervennlige funksjoner og den rå kraften til tradisjonelle Unix-tekstbehandlingsverktøy.
