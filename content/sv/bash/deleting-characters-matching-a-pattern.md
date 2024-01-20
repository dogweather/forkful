---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort tecken som matchar ett mönster handlar om att identifiera specifika sekvenser av tecken, eller ett "mönster", i en textsträng, och ta bort dem. Programerare gör detta för att organisera eller renare data, effektivisera bearbetning och effektiv tokenizering.

## Så här gör du:
I Bash kan du enkelt ta bort matchande tecken med 'tr' -kommandot.

```Bash
echo "Hej från Bash" | tr -d 'a'
```

Detta tar bort alla instanser av tecknet 'a'. Output kommer att vara:

```Bash
Hej frn Bsh
```

Du kan också använda sträng substitution inom Bash:

```Bash
text="Hej från Bash"
echo ${text//a/}
```

Detta tar också bort alla 'a': er. Output kommer återigen att vara:

```Bash
Hej frn Bsh
```

## Djupdykning
Förmågan att ta bort tecken har varit en historisk del av Unix-programmering och har mycket att göra med dess strömorienterade design. 'tr' -kommandot har varit en del av Unix sedan dess tidiga dagar.

Några alternativ till Bash kan vara 'sed', 'awk' och 'perl'. Dessa verktyg erbjuder ett kraftfullt mönsterspråk men kan vara överväldigande för enklare uppgifter. Bash har fördelen att den är lättviktig och integrerad i de flesta Linuxsystem.

Generellt sett, Bash tar bort tecken genom att först jämföra varje tecken i strängen till mönstret. När en matchning hittas, tas tecknet bort och resten av strängen kollapsar för att fylla i luckan.

## Se också
Mer detaljerade guider och information finns på följande sidor:

- [GNU Bash manual](https://www.gnu.org/software/bash/manual/)
- [Komplett guide till 'sed'-kommandot](https://www.grymoire.com/Unix/Sed.html)
- [Tutorial om 'awk'](https://www.tutorialspoint.com/awk/index.htm)