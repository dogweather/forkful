---
date: 2024-01-26 03:47:27.375181-07:00
description: "Att anv\xE4nda en debugger i Bash inneb\xE4r att utnyttja verktyg f\xF6\
  r att testa och hitta problem i dina skript, s\xE5som att f\xE5nga buggar som kraschar\
  \ din kod\u2026"
lastmod: '2024-03-13T22:44:38.086931-06:00'
model: gpt-4-0125-preview
summary: "Att anv\xE4nda en debugger i Bash inneb\xE4r att utnyttja verktyg f\xF6\
  r att testa och hitta problem i dina skript, s\xE5som att f\xE5nga buggar som kraschar\
  \ din kod\u2026"
title: "Att anv\xE4nda en debugger"
---

## Hur man gör:
Bash kommer inte med en inbyggd debugger som vissa andra språk, men du kan använda inbyggda kommandon som `set -x` för att spåra vad som händer. Eller, för en uppgradering, finns det `bashdb`, en riktig debugger för att stega igenom din kod. Här är en titt:

```Bash
# Använder set -x för att felsöka
set -x
echo "Startar felsökning"
my_var="Hej, felsökningsvärlden!"
echo $my_var
set +x

# Använder bashdb
# Installera bashdb med din pakethanterare, t.ex., apt, yum, brew.
# Felsök ett skript som kallas my_script.sh:
bashdb my_script.sh
```

Utskrift när man kör med `set -x`:
```Bash
+ echo 'Startar felsökning'
Startar felsökning
+ my_var='Hej, felsökningsvärlden!'
+ echo 'Hej, felsökningsvärlden!'
Hej, felsökningsvärlden!
+ set +x
```

## Djupdykning
Historiskt sett innebar felsökning av Bash-skript att man strödde koden med `echo`-utskrifter. Men sedan kom `set -x`, som gav oss en titt in i körningsexekveringen utan manuella utskrifter. Och för de som längtar efter mer kontroll dök `bashdb`-debuggern upp, inspirerad av gdb-debuggern för C/C++.

När det gäller alternativ, bortom `set`-kommandona (`-x`, `-v`, `-e`), inkluderar andra alternativ att omdirigera utskriften till en fil för analys eller att använda externa verktyg som ShellCheck för statisk analys.

Implementeringsmässigt är `set -x` enkelt; det är ett inbyggt Bash-alternativ som skriver ut kommandon och deras argument när de utförs. `bashdb`, å andra sidan, tillåter stegning genom kod, att sätta brytpunkter och att evaluera uttryck - saker som ger dig en stridande chans mot mer undvikande buggar.

## Se även
- Bash Debugger Project: http://bashdb.sourceforge.net/
- "Pro Bash Programming" av Chris Johnson och Jayant Varma för avancerad skriptning.
- ShellCheck för statisk analys: https://www.shellcheck.net/
