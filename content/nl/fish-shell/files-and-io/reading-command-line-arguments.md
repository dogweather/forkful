---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:10.181830-07:00
description: "Command line argumenten lezen is het oppakken van de extra bits die\
  \ je typt na de naam van je script, zoals geheime handdrukken om het gedrag van\
  \ een\u2026"
lastmod: '2024-02-25T18:49:48.588496-07:00'
model: gpt-4-0125-preview
summary: "Command line argumenten lezen is het oppakken van de extra bits die je typt\
  \ na de naam van je script, zoals geheime handdrukken om het gedrag van een\u2026"
title: Commandoregelargumenten lezen
---

{{< edit_this_page >}}

## Wat & Waarom?
Command line argumenten lezen is het oppakken van de extra bits die je typt na de naam van je script, zoals geheime handdrukken om het gedrag van een script aan te passen. Programmeurs doen dit om scripts flexibel en interactief te maken zonder gedoe.

## Hoe:

Stel `greet.fish` is je script. Je wilt dat het een naam neemt en een begroeting uitspuwt.

```fish
#!/usr/bin/env fish

# De argumenten worden opgeslagen in $argv
# $argv[1] is het eerste argument, $argv[2] het tweede, en zo verder.

set name $argv[1]
echo "Hallo, $name!"
```

Voer het uit:

```shell
$ fish greet.fish Wereld
Hallo, Wereld!
```

Nu, met meerdere argumenten:

```fish
#!/usr/bin/env fish

# Doorloop alle argumenten
for arg in $argv
    echo "Hallo, $arg!"
end
```

Probeer het:

```shell
$ fish greet.fish Aarde Mars Venus
Hallo, Aarde!
Hallo, Mars!
Hallo, Venus!
```

Om vlaggen te hanteren (zoals `-u` voor hoofdletters):

```fish
#!/usr/bin/env fish

# Controleer op een "-u" argument
set -l uppercase_mode uit
for arg in $argv
    if test "$arg" = "-u"
        set uppercase_mode aan
    else if set -q uppercase_mode[1]; and string match --quiet -- "$uppercase_mode" "aan"
        echo (string upper "$arg")
    else
        echo $arg
    end
end
```

En roep aan:

```shell
$ fish greet.fish -u mercurius venus
MERCURIUS
VENUS
```

## Diepere duik

Fish Shell heeft al lange tijd command line argumenten goed onder de knie, net als andere shells. Wat Fish onderscheidt, is zijn eenvoud door ontwerp. Er zijn geen `$1, $2... $n` om te onthouden; het is een array `$argv`, bekend terrein als je in andere programmeertalen duikt.

Er zijn alternatieven, zoals bash, zsh, enz., maar de scripttaal van Fish streeft ernaar leesbaarder en eenvoudiger te zijn. In plaats van traditionele `shift` commando's of te maken hebben met `$@` voor alle argumenten, heeft Fish dat vriendelijke `$argv` en mooie scriptconstructies zoals `for` lussen en `if` condities die minder over cryptische symbolen gaan en meer over duidelijke woorden.

Bij de implementatie is het van vitaal belang om te overwegen hoe je script gebruikt zal worden. Zijn er standaardwaarden nodig? Zullen gebruikers weten wat ze moeten invoeren? Zorg ervoor dat je gevallen aanpakt waar gebruikers vergeten argumenten door te geven of ze in de verkeerde volgorde passeren.

## Zie ook

- De officiële Fish documentatie over command line argumenten: [fishshell.com/docs/current/#syntax-command-line](https://fishshell.com/docs/current/#syntax-command-line)
- Voor geavanceerd scripten en het creëren van je eigen functies in Fish: [fishshell.com/docs/current/#defining-functions](https://fishshell.com/docs/current/#defining-functions)
- Een introductie tot Fish voor gebruikers met een achtergrond in andere shells: [fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
