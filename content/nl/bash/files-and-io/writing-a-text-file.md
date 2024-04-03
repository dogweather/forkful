---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:22.944482-07:00
description: "Een tekstbestand schrijven is het proces van het opslaan van gegevens\
  \ in een bestand in tekstformaat. Programmeurs doen dit om configuraties, logs,\
  \ code\u2026"
lastmod: '2024-03-13T22:44:51.002163-06:00'
model: gpt-4-0125-preview
summary: Een tekstbestand schrijven is het proces van het opslaan van gegevens in
  een bestand in tekstformaat.
title: Een tekstbestand schrijven
weight: 24
---

## Wat & Waarom?

Een tekstbestand schrijven is het proces van het opslaan van gegevens in een bestand in tekstformaat. Programmeurs doen dit om configuraties, logs, code of andere gegevens die gerefereerd of bewaard moeten worden over tijd op te slaan.

## Hoe:

```Bash
# Een nieuw tekstbestand maken met het 'echo'-commando
echo "Hallo, wereld!" > hallo.txt

# Meer tekst toevoegen aan een bestaand bestand met de '>>'-operator
echo "Nog een regel tekst." >> hallo.txt

# Meerdere regels schrijven met een heredoc
cat << EOF > hallo_meerregelig.txt
Hallo, dit is de eerste regel.
En dit is de tweede regel.
EOF
```

Uitvoer voor `cat hallo.txt`:
```
Hallo, wereld!
Nog een regel tekst.
```

Uitvoer voor `cat hallo_meerregelig.txt`:
```
Hallo, dit is de eerste regel.
En dit is de tweede regel.
```

## Diepere duik

Shell scripting is sinds de jaren 70 een kernonderdeel van Unix-achtige systemen, met de `sh` (Bourne shell) als de originele. Tegenwoordig is `bash` (Bourne Again SHell) een breed beschikbare en gebruikte shell. Hoewel `echo` en uitvoeromleiding (`>`, `>>`) gangbare methoden zijn om bestanden te schrijven, bieden alternatieven zoals `printf` opmaakmogelijkheden. Bestandsschrijven in bash-scripts gebruikt bestandsbeschrijvers; `1` voor `stdout`, en toevoegen (`>>`) voorkomt het overschrijven van bestanden door gebruik te maken van bestandsbeschrijver `2`.

## Zie ook

- [GNU Bash-handleiding](https://www.gnu.org/software/bash/manual/bash.html)
- [Geavanceerde Bash-scriptinggids](https://www.tldp.org/LDP/abs/html/)
- [Shell Scripting Tutorial](https://www.shellscript.sh/)
