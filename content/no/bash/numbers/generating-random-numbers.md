---
date: 2024-01-27 20:32:37.178554-07:00
description: "\xC5 generere tilfeldige tall i Bash gir en m\xE5te \xE5 innf\xF8re\
  \ uforutsigbarhet i skript p\xE5, noe som er essensielt for oppgaver som \xE5 generere\
  \ sikre passord,\u2026"
lastmod: '2024-03-13T22:44:40.968256-06:00'
model: gpt-4-0125-preview
summary: "\xC5 generere tilfeldige tall i Bash gir en m\xE5te \xE5 innf\xF8re uforutsigbarhet\
  \ i skript p\xE5, noe som er essensielt for oppgaver som \xE5 generere sikre passord,\
  \ simulere data eller for programmering av spill."
title: Generering av tilfeldige tall
weight: 12
---

## Hva og hvorfor?
Å generere tilfeldige tall i Bash gir en måte å innføre uforutsigbarhet i skript på, noe som er essensielt for oppgaver som å generere sikre passord, simulere data eller for programmering av spill. Programmerere utnytter denne muligheten til å legge til variabilitet i skriptene sine eller for å teste programmene sine under en rekke tilfeldig genererte forhold.

## Hvordan gjøre det:
I Bash er `$RANDOM`-variabelen veien å gå for å generere tilfeldige tall. Hver gang du refererer til den, gir Bash et pseudotilfeldig heltall mellom 0 og 32767. La oss utforske noen praktiske eksempler:

```Bash
# Grunnleggende bruk av $RANDOM
echo $RANDOM

# Generere et tilfeldig tall innenfor et spesifisert område (0-99 her)
echo $(( RANDOM % 100 ))

# Generere et mer "sikkert" tilfeldig tall, egnet for passord eller nøkler
# Bruker /dev/urandom med od-kommando
head -c 8 /dev/urandom | od -An -tu4

# Såing av RANDOM for reproduserbarhet
RANDOM=42; echo $RANDOM
```

Eksempel på utskrift (merk: faktisk utskrift vil variere siden tallene er tilfeldige):
```Bash
16253
83
3581760565
17220
```

## Dypdykk
Mekanismen bak Bashs `$RANDOM` genererer pseudotilfeldige tall, noe som betyr at de følger en algoritme og kan, i teorien, være forutsigbare - en potensiell sikkerhetsfeil for applikasjoner som krever ekte uforutsigbarhet. Moderne kryptografiske applikasjoner krever vanligvis tilfeldighet avledet fra fysiske fenomener eller fra maskinvare spesielt designet for å generere tilfeldige data, som `/dev/urandom` eller `/dev/random` i Linux, som samler miljøstøy.

For tilfeldige eller ikke-sikkerhetskritiske oppgaver, er `$RANDOM` tilstrekkelig og tilbyr fordelen av enkelhet. Imidlertid, for kryptografiske formål eller der kvaliteten på tilfeldigheten er kritisk, bør utviklere se mot andre verktøy og språk designet med kryptografi i tankene, som OpenSSL eller programmeringsspråk med robuste biblioteker for generering av tilfeldige tall.

Selv om Bashs `$RANDOM` tjener formålet i skript som krever grunnleggende tilfeldige tall, bør dens begrensninger styre utviklere mot mer robuste løsninger for applikasjoner der kvaliteten eller sikkerheten til tilfeldigheten er viktig.
