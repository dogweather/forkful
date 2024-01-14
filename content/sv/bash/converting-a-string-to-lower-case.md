---
title:                "Bash: Omvandla en sträng till gemener"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till varför du skulle vilja konvertera en sträng till gemener (lower case) i Bash-programmering. En möjlig anledning kan vara för att jämföra strängar på ett mer enhetligt sätt, oavsett vilken stor bokstav de innehåller.

## Så här gör du

För att konvertera en sträng till gemener i Bash kan du använda kommandot `tr [a-z] [A-Z]` följt av det du vill konvertera. Till exempel: 

```Bash
echo "HELLO WORLD" | tr [A-Z] [a-z]
```

Detta kommer att ge utdatan "hello world". Om du vill spara utdatan i en variabel kan du använda följande kod:

```Bash
name="JOHN DOE"
lowercase_name=$(echo $name | tr [A-Z] [a-z])
echo $lowercase_name
```

Detta kommer att ge utdatan "john doe". Det är viktigt att notera att `tr`-kommandot bara konverterar bokstäver i det engelska alfabetet. Om du har specialtecken eller bokstäver från andra språk kan du behöva använda en annan metod för konvertering.

## Djupdykning

Bakom kulisserna använder `tr`-kommandot sig av ASCII-tabellen för att byta ut stora bokstäver mot små bokstäver. ASCII står för American Standard Code for Information Interchange och är ett vanligt använd format för att representera text i datorer. Varje bokstav har en numerisk värde som `tr`-kommandot refererar till för konverteringen.

## Se även

- [Officiell dokumentation för `tr`](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html#tr-invocation)
- [Mer information om ASCII-tabellen](https://www.ascii-code.com/)
- [En guide om andra sätt att konvertera bokstäver i Bash](https://askubuntu.com/questions/488875/is-there-a-way-to-create-an-alphabetically-rearranged-list-of-all-the-english-l)