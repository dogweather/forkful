---
aliases:
- /nl/fish-shell/handling-errors/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:45.993610-07:00
description: "Foutafhandeling stelt je script in staat om elegant om te gaan met het\
  \ onverwachte. We doen dit om falen te beheren zonder onze gebruiker grijs haar\
  \ te\u2026"
lastmod: 2024-02-18 23:09:02.334274
model: gpt-4-0125-preview
summary: "Foutafhandeling stelt je script in staat om elegant om te gaan met het onverwachte.\
  \ We doen dit om falen te beheren zonder onze gebruiker grijs haar te\u2026"
title: Fouten afhandelen
---

{{< edit_this_page >}}

## Wat & Waarom?
Foutafhandeling stelt je script in staat om elegant om te gaan met het onverwachte. We doen dit om falen te beheren zonder onze gebruiker grijs haar te bezorgen.

## Hoe te:
Om fouten in Fish op te vangen, vertrouw op de `status` opdracht en conditionals. Stel dat `ping` faalt; hier is hoe je dat detecteert:

```fish
ping -c 1 example.com
if not status is-success
    echo "Er is iets fishy gebeurd met de ping."
end
```

Voorbeelduitvoer als `ping` faalt:

```
Er is iets fishy gebeurd met de ping.
```

Om een specifieke foutcode te behandelen, gebruik `status --is`:

```fish
false
if status --is 1
    echo "Een fout met code 1 gevangen."
end
```

Voorbeelduitvoer:
```
Een fout met code 1 gevangen.
```

Voor een robuustere aanpak, overweeg het gebruik van een functie:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping mislukt met status $status"
        return 1
    end
end

try_ping
```

## Diepe Duik
Foutafhandeling in Fish komt niet overeen met het `try/catch` paradigma dat je misschien kent van hogere programmeertalen. In plaats daarvan heb je te maken met eenvoudige exitstatuses die worden verstrekt door de `status` opdracht.

Historisch gezien betekent in Unix-achtige systemen een exitstatus van `0` succes, terwijl elke niet-nul waarde een fout aangeeft, wat vaak verschillende redenen voor falen weerspiegelt. Deze conventie wordt gebruikt door de meeste command-line hulpprogramma's en dus ook door Fish zelf.

Alternatieven voor `status` checks in Fish omvatten signal handling via `trap` in andere shells, maar Fish geeft de voorkeur aan explicietere statuscontroles, omdat het schoner is en minder vatbaar voor bijeffecten.

Wat betreft de implementatie, blijft foutafhandeling in Fish eenvoudig maar krachtig, grotendeels te danken aan zijn non-blokkerende aard en nadruk op duidelijke syntaxis, zoals getoond in de voorbeelden. Foutcodes gaan mooi samen met functies, wat zorgt voor modulaire en leesbare foutbeheersing.

## Zie Ook
- Fish documentatie over conditionals: https://fishshell.com/docs/current/language.html#conditionals
- Fish tutorial over foutafhandeling: https://fishshell.com/docs/current/tutorial.html#error-handling
