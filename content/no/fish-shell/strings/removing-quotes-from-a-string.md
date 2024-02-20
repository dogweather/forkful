---
date: 2024-01-26 03:39:07.856319-07:00
description: "\xC5 fjerne anf\xF8rselstegn fra en streng handler om \xE5 stripe bort\
  \ de irriterende enkle (' ') eller doble (\" \") anf\xF8rselstegnene fra tekstdataene\
  \ dine.\u2026"
lastmod: 2024-02-19 22:05:00.491640
model: gpt-4-0125-preview
summary: "\xC5 fjerne anf\xF8rselstegn fra en streng handler om \xE5 stripe bort de\
  \ irriterende enkle (' ') eller doble (\" \") anf\xF8rselstegnene fra tekstdataene\
  \ dine.\u2026"
title: "Fjerne anf\xF8rselstegn fra en streng"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å fjerne anførselstegn fra en streng handler om å stripe bort de irriterende enkle (' ') eller doble (" ") anførselstegnene fra tekstdataene dine. Programmerere gjør ofte dette for å rense input eller forberede data for videre behandling uten rotet med anførselstegn.

## Hvordan:

Fish har innebygd magi for denne typen oppgave. Bruk `string`-funksjonen uten å svette. Sjekk ut disse trylleformlene:

```fish
# Eksempel med enkle anførselstegn
set quoted "'Hei, Verden!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Utdata: Hei, Verden!

# Samme greie med doble anførselstegn
set double_quoted "\"Hei, Univers!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Utdata: Hei, Univers!
```

## Dypdykk

Tilbake i kommandolinjens steinalder, ville du brydd deg med `sed` eller `awk` for å strippe anførselstegn; et virkelig virvar av omvendte skråstreker og kryptiske flagg. Fish sin `string`-funksjon er fra en nyere æra, som gjør kode renere og mer intuitiv.

Alternativer i andre skall kunne fortsatt stole på disse gamle verktøyene, eller kunne bruke sine egne innebygde metoder som bash sin parameter-ekspansjon eller zsh sine modifikatorer.

`String`-funksjonen går utover å trimme anførselstegn. Det er en sveitsisk armékniv for streng-operasjoner i Fish. Med `string` kan du skjære, dele, splitte, sammenføye, eller til og med regex-matche strenger rett i terminalen din.

## Se Også

Dykk dypere inn i `string` med hjelp av den offisielle dokumentasjonen:
- [Fish Shell Strengdokumentasjon](https://fishshell.com/docs/current/commands.html#string)

For nostalgi eller når du skripter med mer tradisjonelle skall, sjekk ut:
- [Sed & Awk Guide](https://www.grymoire.com/Unix/Sed.html)
- [Bash Parameter-ekspansjon](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
