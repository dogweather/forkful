---
date: 2024-01-26 04:08:56.813966-07:00
description: "\"\xC5 bruke en feils\xF8ker\" handler om \xE5 steg-for-steg g\xE5 gjennom\
  \ Python-koden din for \xE5 avdekke feil og forst\xE5 oppf\xF8rsel. Vi gj\xF8r det\
  \ fordi det er mye enklere\u2026"
lastmod: '2024-02-25T18:49:38.593784-07:00'
model: gpt-4-0125-preview
summary: "\"\xC5 bruke en feils\xF8ker\" handler om \xE5 steg-for-steg g\xE5 gjennom\
  \ Python-koden din for \xE5 avdekke feil og forst\xE5 oppf\xF8rsel. Vi gj\xF8r det\
  \ fordi det er mye enklere\u2026"
title: "\xC5 bruke en debugger"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
"Å bruke en feilsøker" handler om å steg-for-steg gå gjennom Python-koden din for å avdekke feil og forstå oppførsel. Vi gjør det fordi det er mye enklere enn å bare gjette hvor ting gikk galt, og det sparer oss for timer av print-setning-purgatoriet.

## Hvordan:
La oss bryte ned bruken av `pdb`, Pythons innebygde feilsøker. Forestill deg en fil, `buggy.py`, med en lur feil:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

Når du kjører dette skriptet, forventer du `8`, men det kaster bare en syntaksfeil. Det er på tide med feilsøkeren!

I terminalen din, kjør:
```bash
python -m pdb buggy.py
```

Du vil entre feilsøkeren, og den ser slik ut:
```Python
> /sti_til_fil/buggy.py(1)<modul>()
-> def add_one(number):
```

Bruk `l(ist)` for å se mer kode, `n(ext)` for å gå til neste linje, eller `c(ontinue)` for å fortsette å kjøre skriptet. Når du treffer feilen, vil `pdb` stoppe og la deg inspisere.

Etter at du har rettet `number ++ 1` til `number + 1`, restart feilsøkeren for å teste fiksen.
Husk, venner lar ikke venner kode uten et nett. Nok sagt.

## Dypdykk
Tilbake i programmeringens mørke tidsalder (også kjent som før integrerte utviklingsmiljøer, eller IDEer, var overalt), var feilsøkere ofte selvstendige verktøy du brukte utenfor teksteditoren din. De kom til unnsetning ved å la programmerere inspisere tilstanden til programvaren deres på forskjellige utførelsespunkter.

Per 2023, er ikke Pythons `pdb` det eneste alternativet. Folk kan bruke IDEer som PyCharm eller Visual Studio Code, som har sine egne glatte feilsøkere innebygd. Disse legger til hendige funksjoner som breakpoints du kan sette med et klikk, i stedet for å taste inn kryptiske kommandoer.

Så har vi `ipdb`, en pakke som kan installeres med pip og som bringer `IPython`-godheten til feilsøking. Det er som `pdb` på ytelsesfremmende midler, med tab-completion og syntaksutheving.

Feilsøkere varierer også i deres implementasjon. Noen går tett innpå programutførelsen på maskin- eller bytekode-nivå. Andre, som mange feilsøkere for høynivåspråk, kjører koden i et spesielt miljø som overvåker variabeltilstander og kontrollerer utførelsesflyten.

## Se Også
For den fulle oversikten over Pythons egen feilsøker, sjekk ut:
- `pdb`-dokumentasjonen: https://docs.python.org/3/library/pdb.html

Hvis du er nysgjerrig på alternativer, vil disse lenkene tjene deg godt:
- `ipdb`-lagringssted og brukerguide: https://github.com/gotcha/ipdb
- Debugging med Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- PyCharm-feilsøkingsfunksjoner: https://www.jetbrains.com/help/pycharm/debugging-code.html

Lykke til med feiljakten!
