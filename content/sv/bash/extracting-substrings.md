---
title:                "Bash: Extrahering av understrängar"
simple_title:         "Extrahering av understrängar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

En av de grundläggande funktionerna i Bash-programmering är att extrahera substrängar (delsträngar) från en större textsträng. Detta kan vara användbart för att manipulera data eller för att bearbeta text på ett specifikt sätt. 

## Så här gör du

För att extrahera en substräng i Bash använder du kommandot `cut`. Det här kommandot tar ett antal flaggor som styr utmatningen och visar bara den del av texten som du vill ha. Här är ett enkelt exempel:

```Bash
text="Hej! Det här är en textsträng."
echo ${text:9:4}
```

**Utanför kodblocket** betyder detta att vi har en variabel `text` som innehåller en textsträng. I den sista raden använder vi sedan `echo` för att skriva ut en del av texten, från position 9 till längden 4. Detta ger oss ett resultat på "här ".

En annan användbar funktion är att kunna extrahera en del av en textsträng baserat på ett visst villkor. I det här fallet kan du använda `grep`-kommandot för att söka igenom en textsträng och bara returnera de delar som matchar ditt villkor. Här är ett exempel:

```Bash
text="Hej! Det här är en textsträng."
echo $text | grep "här"
```

I detta fall blir resultatet "här är en textsträng."

## Djupdykning

När det gäller att extrahera substrängar i Bash, finns det många olika alternativ och flaggor som du kan använda för att anpassa din utmatning. Till exempel kan du använda `-f` flaggan med `cut`-kommandot för att specificera vilka tecken som ska användas som avgränsare i din textsträng. Detta kan vara användbart om du exempelvis arbetar med en CSV-fil.

För att gå ännu djupare i ämnet kan du också använda regelbundna uttryck för att extrahera delsträngar. Under övningens gång kan du upptäcka att det finns vissa mönster i textsträngen som du vill extrahera. I dessa fall kan du använda verktyg som `sed` eller `awk` för att skapa mer avancerade uttryck som hjälper dig att få den exakta delsträngen du vill ha.

## Se också

- [Dokumentation för `cut`](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html)
- [Dokumentation för `grep`](https://www.gnu.org/software/grep/manual/grep.html)
- [Dokumentation för `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Dokumentation för `awk`](https://www.gnu.org/software/gawk/manual/gawk.html)