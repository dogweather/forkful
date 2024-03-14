---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:49.211927-07:00
description: "Bestanden in-place bewerken met CLI one-liners gaat over het direct\
  \ aanbrengen van wijzigingen in bestanden vanaf de opdrachtregel, zonder ze in een\u2026"
lastmod: '2024-03-13T22:44:51.243139-06:00'
model: gpt-4-0125-preview
summary: "Bestanden in-place bewerken met CLI one-liners gaat over het direct aanbrengen\
  \ van wijzigingen in bestanden vanaf de opdrachtregel, zonder ze in een\u2026"
title: Bestanden ter plekke bewerken met CLI one-liners
---

{{< edit_this_page >}}

## Wat & Waarom?

Bestanden in-place bewerken met CLI one-liners gaat over het direct aanbrengen van wijzigingen in bestanden vanaf de opdrachtregel, zonder ze in een teksteditor te openen. Programmeurs doen dit om tijd te besparen en repetitieve bewerkingstaken te automatiseren, wat hun workflow soepeler en efficiënter maakt.

## Hoe:

Fish Shell, bekend om zijn gebruiksvriendelijke functies en krachtige scriptmogelijkheden, biedt verschillende manieren om bestanden in-place te bewerken. Echter, in tegenstelling tot sommige andere shells, heeft Fish geen ingebouwd mechanisme voor in-place bewerking (`sed -i` in Bash, bijvoorbeeld). Maar vrees niet, je kunt dit nog steeds bereiken met een beetje creativiteit en wat hulp van externe tools zoals `sed` en `awk`.

### Gebruikmaken van `sed` voor eenvoudige vervangingen
Om alle instanties van "hallo" te vervangen door "wereld" in `file.txt`, zou je gebruiken:
```Fish Shell
sed -i '' 's/hallo/wereld/g' file.txt
```

### Meerdere `sed` commando's toepassen
Als je meerdere vervangingen moet uitvoeren, kun je ze zo ketenen:
```Fish Shell
sed -i '' -e 's/vis/baars/g' -e 's/regenboog/forel/g' file.txt
```

### Gebruikmaken van `awk` voor complexere bewerkingen
Voor bewerkingen die te complex zijn voor `sed`, is `awk` mogelijk het hulpmiddel bij uitstek. Hier is hoe je het getal op elke regel kunt verdubbelen:
```Fish Shell
awk '{print $1 * 2}' file.txt > tijdelijk && mv tijdelijk file.txt
```

### Opmerking over Foutafhandeling
Onthoud, bij het gebruik van deze tools vanuit Fish, het vastleggen van fouten en het begrijpen van hun berichten is cruciaal. Gebruik Fish’s solide foutafhandeling om je scripts betrouwbaarder te maken.

## Diepgaande duik

Historisch gezien is in-place bestandsbewerking een basis van Unix- en Linux-programmering geweest, die een efficiënte manier biedt om snel wijzigingen aan te brengen zonder bestanden handmatig te openen. Hulpmiddelen zoals `sed` en `awk` zijn vereerde hulpmiddelen die al sinds de vroege dagen van Unix bestaan en onmisbaar zijn geworden voor tekstverwerkingsopdrachten.

Fish Shell, hoewel moderner en met verbeteringen in gebruiksvriendelijkheid en scripting, mist ingebouwde in-place bewerking voornamelijk vanwege zijn ontwerpfilosofie gericht op interactiviteit en gebruiksvriendelijkheid. De afwezigheid van een native in-place bewerkingscommando in Fish onderstreept het belang van externe hulpmiddelen in Unix-achtige ecosystemen.

Alternatieven voor in-place bewerking in Fish omvatten het gebruik van tijdelijke bestanden of het inzetten van Perl- of Python one-liners, die meer flexibiliteit of leesbaarheid kunnen bieden voor complexe taken.

Bijvoorbeeld, met behulp van Perl:
```Fish Shell
perl -pi -e 's/vind/vervang/g' file.txt
```
Of Python:
```Fish Shell
python -c "import re, sys; [sys.stdout.write(re.sub('patroon', 'vervanging', regel)) for regel in sys.stdin]" < file.txt > tijdelijk && mv tijdelijk file.txt
```

Wat betreft de implementatie, wanneer je in-place bewerkingen uitvoert, creëren deze tools doorgaans een tijdelijk bestand, schrijven de wijzigingen daarin en vervangen dan het originele bestand met de gewijzigde versie. Deze aanpak zorgt ervoor dat het bestandsbewerkingsproces geen gegevens corrupt maakt of verliest als er tijdens de operatie een fout optreedt.

Het begrijpen van deze hulpmiddelen en methoden stelt Fish Shell-programmeurs in staat om in-place bewerkingen effectief in hun scripts op te nemen, waardoor de kloof tussen Fish's gebruiksvriendelijke functies en de rauwe kracht van traditionele Unix-tekstverwerkingshulpmiddelen wordt overbrugd.
