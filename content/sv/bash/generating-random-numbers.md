---
title:                "Skapa slumpmässiga nummer"
html_title:           "Bash: Skapa slumpmässiga nummer"
simple_title:         "Skapa slumpmässiga nummer"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumpmässiga nummer är känt som en viktig del av programmering. Det innebär att skapa nummer som inte följer något förutbestämt mönster. Programmerare använder slumpmässiga nummer av olika skäl, till exempel för att simulera spel eller generera slumpmässiga data för testning av sina program.

## Hur gör man:

```Bash
# Generera ett slumpmässigt heltal mellan 1 och 10
echo $((RANDOM % 10 + 1))

# Generera ett slumpmässigt flyttal mellan 0 och 1
echo $((RANDOM / 32767.0))

# Generera en lista med 5 slumpmässiga ord från en textfil
shuf -n 5 textfil.txt
```

Output:
```
7
0.6597
äpple
boll
giraff
skrivare
```

## Djupdykning:

Slumpmässiga nummer har funnits sedan starten av datorer och är en viktig del av många programmeringsspråk, inklusive Bash. Till en början användes pseudoslumpmässiga nummer, som är beräknade nummer som kan låta som slumpmässiga men egentligen följer ett förutbestämt mönster. Idag används mer avancerade algoritmer för att generera verkligt slumpmässiga nummer.

Det finns flera sätt att generera slumpmässiga nummer, inklusive internt i Bash med kommandot ```$RANDOM``` eller med hjälp av externa program som ```shuf``` eller ```od```.

## Se även:

- [Bash dokumentation om variabel $RANDOM](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#index-RANDOM_002c-shell-variable)
- [Wikipedia sida om slumpmässiga nummer](https://sv.wikipedia.org/wiki/Slumpm%C3%A4ssiga_nummer)