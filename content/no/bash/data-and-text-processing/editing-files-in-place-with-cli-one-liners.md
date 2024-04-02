---
date: 2024-01-27 16:20:46.270942-07:00
description: "Tenk deg at du nettopp har funnet ut at du trenger \xE5 gj\xF8re en\
  \ masseoppdatering p\xE5 flere konfigurasjonsfiler som ligger p\xE5 serveren din.\
  \ Du kunne ha \xE5pnet\u2026"
lastmod: '2024-03-13T22:44:40.970254-06:00'
model: gpt-4-0125-preview
summary: "Tenk deg at du nettopp har funnet ut at du trenger \xE5 gj\xF8re en masseoppdatering\
  \ p\xE5 flere konfigurasjonsfiler som ligger p\xE5 serveren din. Du kunne ha \xE5\
  pnet\u2026"
title: "Redigering av filer p\xE5 stedet med CLI-enlinjerskommandoer"
weight: 32
---

## Hva & Hvorfor?

Tenk deg at du nettopp har funnet ut at du trenger å gjøre en masseoppdatering på flere konfigurasjonsfiler som ligger på serveren din. Du kunne ha åpnet hver fil, gjort endringene manuelt og lagret dem. Eller, du kan utføre redigering på stedet direkte fra kommandolinjegrensesnittet (CLI), en ferdighet som sparer tid, reduserer feil og automatiserer repeterende oppgaver. Denne teknikken er spesielt nyttig for systematiske oppdateringer, korreksjoner eller massemodifikasjoner der manuelle redigeringer kunne være upraktiske eller utsatt for feil.

## Hvordan:

Når det kommer til redigering av filer på stedet ved bruk av Bash, kommer to fremtredende verktøy i spill: `sed` og `awk`. La oss utforske hvordan vi kan bruke disse kraftige verktøyene med noen kodeeksempler.

### Bruke `sed` for enkel tekstutskifting

Følgende kommando erstatter den første forekomsten av "text1" med "text2" i `file.txt`:

```Bash
sed -i 's/text1/text2/' file.txt
```

For global erstatning (alle forekomster), ville du lagt til en `g` på slutten:

```Bash
sed -i 's/text1/text2/g' file.txt
```

For å endre flere filer samtidig:

```Bash
sed -i 's/text1/text2/g' file1.txt file2.txt file3.txt
```

### Bruke `awk` for mer komplekse manipulasjoner

`awk` er et annet verktøy som utmerker seg med sine programmeringsmuligheter, spesielt nyttig for tekstbehandling som involverer data basert på felt.

Endre det andre feltet på hver linje til `newValue` i `data.csv`, separert med komma:

```Bash
awk -i inplace -F, '{$2="newValue"; print $0}' OFS=, data.csv
```

### Sikkerhetskopi før du tar spranget

Et praktisk råd: alltid opprett en sikkerhetskopi før redigering på stedet. `sed` letter dette med `-i`-alternativet etterfulgt av et suffiks for å opprette en sikkerhetskopi.

```Bash
sed -i.bak 's/text1/text2/g' file.txt
```

Denne kommandoen oppretter en sikkerhetskopi av den opprinnelige `file.txt` som `file.txt.bak` før erstatningen blir utført.

## Dypdykk

Evnen til å redigere filer direkte fra kommandolinjen dukket opp som en naturlig progresjon av Unix-filosofien: å gi brukere muligheten til å effektivt håndtere og manipulere data med så få tastetrykk som mulig. Likevel, denne makten kommer med sine forbehold.

### Historisk kontekst

Unix-verktøy som `sed` og `awk` har vært rundt siden Unix' tidlige dager, skapt som en del av dens verktøykassefilosofi, som fokuserer på spesialiserte, sammenstilbare kommandoer. Deres inklusjon i Unix' arsenal var et svar på behovet for effektiv tekstbehandling i et landskap dominert av kommandolinjegrensesnitt.

### Alternativer

Selv om `sed` og `awk` er kraftige, er de ikke de eneste alternativene. Perl og Python, for eksempel, har kommandolinjealternativer (`-p` og `-i`, henholdsvis) som tillater lignende redigering på stedet med en muligens mer lesbar syntaks for komplekse operasjoner.

```Bash
perl -pi -e 's/text1/text2/g' file.txt
```

```Bash
python -c "import fileinput, sys; [sys.stdout.write(line.replace('text1', 'text2')) for line in fileinput.input(files='file.txt', inplace=True)]"
```

Hvert alternativ har sine styrker: Perl sine kommando-i-ett-linje-evner er enorme, og Pythons syntaks er muligens mer tilgjengelig for de som ikke er dypt kjent med Unix tekstbehandlingsverktøy.

### Gjennomføringsdetaljer

Redigering på stedet er ikke virkelig "på stedet" i teknisk forstand. Både `sed -i` og `awk -i inplace` fungerer ved å opprette en midlertidig fil hvor det bearbeidede utdataet lagres før det erstatter den originale filen. Denne tilnærmingen sikrer at filen ikke blir korrupt hvis prosessen blir avbrutt. Implikasjonene er hovedsakelig på ressurser og tillatelser: du må ha nok diskplass for den midlertidige filen og tillatelser til å skape filer i mappen til målfilen din.

Selv om kraftige, må redigeringskommandoer på stedet brukes med forsiktighet. En feilplassert regex kan føre til tap av data, noe som understreker viktigheten av sikkerhetskopier. Til tross for potensielle fallgruver, kan mestring av disse kommandoene betydelig øke din evne til å utføre raske, effektive filmodifikasjoner direkte fra kommandolinjen, som kroppsliggjør Unix-filosofien om å utnytte enkle, kraftige verktøy for å fullføre komplekse oppgaver.
