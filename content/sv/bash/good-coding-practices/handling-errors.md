---
date: 2024-01-26 00:49:33.730980-07:00
description: "Att hantera fel i Bash-scripting handlar om att f\xF6rutse var saker\
  \ kan g\xE5 fel och hantera det p\xE5 ett smidigt s\xE4tt. Varf\xF6r? Jo, det h\xE5\
  ller ditt skript\u2026"
lastmod: '2024-03-13T22:44:38.089715-06:00'
model: gpt-4-1106-preview
summary: "Att hantera fel i Bash-scripting handlar om att f\xF6rutse var saker kan\
  \ g\xE5 fel och hantera det p\xE5 ett smidigt s\xE4tt. Varf\xF6r? Jo, det h\xE5\
  ller ditt skript\u2026"
title: Hantering av fel
weight: 16
---

## Vad & Varför?

Att hantera fel i Bash-scripting handlar om att förutse var saker kan gå fel och hantera det på ett smidigt sätt. Varför? Jo, det håller ditt skript robust och sparar användare från huvudbry när saker inte fungerar som förväntat.

## Så här gör man:

```Bash
#!/bin/bash

# Omdirigera stderr till en fil
grep "något" fil.txt 2> fel.log

# Felhantering med utgångsstatusar
if ! grep "något" fil.txt; then
    echo "Hoppsan, något gick fel vid sökningen efter 'något'."
    exit 1
fi

# Använda en fälla för att städa innan avslut vid fel
cleanup() {
  echo "Rensar upp temporära filer..."
  rm temp_*
}

trap cleanup ERR

# avsiktligt fel: filen finns inte
cat temp_fil.txt
```

Exempel på utmatning när ett fel inträffar:

```
Rensar upp temporära filer...
cat: temp_fil.txt: Filen eller katalogen finns inte
```

## Fördjupning

Felhantering i Bash-scriptning går tillbaka till Unix-skalets ursprung, där robusta och tillförlitliga skript var (och är) avgörande för systemadministration och automatisering. Traditionellt hanteras fel i Bash genom att kontrollera utgångsstatusen för ett kommando, som enligt konventionen returnerar 0 för framgång och ett icke-nollvärde för misslyckande.

Bash introducerade `trap`-kommandot som inbyggt, vilket tillåter användare att specificera kommandon att köra på olika signaler eller skriptavslut. Detta är användbart för rensningsuppgifter eller som en sista utväg för felhantering.

Det finns också `set`-kommandot, som kan ändra beteendet för Bash vid fel. Till exempel kommer `set -e` att få ett skript att avslutas omedelbart om något kommando avslutas med en icke-noll status, ett sätt att snabbt misslyckas och undvika kedjereaktioner av fel.

Alternativ till Bash inbyggda felhantering inkluderar att explicit kontrollera existensen av filer, använda kommandosubstitution, eller till och med skriva egna funktioner för att hantera fel mer detaljerat.

Även om noggrann felhantering ibland kan kännas överdrivet för små skript, är det en praxis som kan spara mycket tid på felsökning och förhindra oväntat beteende både för dig och användarna.

## Se också

- Bash-manualen om skalparametrar: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Avancerad Bash-scripting guide's avsnitt om Felhantering: https://www.tldp.org/LDP/abs/html/exit-status.html
- En ingående guide till `trap`: https://mywiki.wooledge.org/SignalTrap

Kom ihåg, skriptning är en konstform, och hur du hanterar misstag och snubblingar kan göra ditt mästerverk mer resilient. Lycklig skriptning!
