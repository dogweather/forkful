---
date: 2024-01-26 04:08:56.813966-07:00
description: "La oss bryte ned bruken av `pdb`, Pythons innebygde feils\xF8ker. Forestill\
  \ deg en fil, `buggy.py`, med en lur feil: ```Python def add_one(number): result\
  \ =\u2026"
lastmod: '2024-03-13T22:44:40.365877-06:00'
model: gpt-4-0125-preview
summary: "La oss bryte ned bruken av `pdb`, Pythons innebygde feils\xF8ker."
title: "\xC5 bruke en debugger"
weight: 35
---

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
