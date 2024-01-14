---
title:                "Fish Shell: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Varför 
Att jämföra två datum är en användbar funktion inom Fish Shell som kan hjälpa dig att organisera och hantera dina filer och projekt bättre. Det kan även vara användbart för att säkerställa att du inte råkar arbeta med föråldrade filer eller glömmer viktiga frister.

# Hur man gör
För att börja jämföra två datum i Fish Shell, behöver du använda kommandot "date" följt av de två datum du vill jämföra. Se till att skriva datumen i formatet "ÅÅÅÅ/MM/DD" för att undvika förvirring.

```Fish Shell
date 2019/05/01
date 2020/10/15
```

Detta kommer att returnera ett resultat som jämför de två datumen och visar vilket av dem som är tidigare.

```
ons 01 maj 2019 00:00:00 CEST
tor 15 okt 2020 00:00:00 CEST
```

Det är viktigt att notera att Fish Shell använder ditt lokala tidsformat för att visa resultatet, så resultatet kan se annorlunda ut beroende på var du befinner dig i världen.

# Djupdykning
Förutom att bara jämföra två datum kan du också använda Fish Shell för att beräkna skillnaden mellan dem. Detta kan vara särskilt användbart när du behöver hålla koll på hur många dagar som återstår till en viktig deadline.

Ett exempel på hur du kan göra detta är genom att använda kommandot "date -d" följt av datumen du vill jämföra. Resultatet kommer att visa hur många dagar det är mellan datumen.

```Fish Shell
date -d 2020/01/01 -d 2020/01/15
```

```
14
```

Du kan också göra mer komplicerade jämförelser genom att använda standard Unix-verktyg som "diff" eller "grep" tillsammans med "date" kommandot.

# Se även
Här är några användbara länkar för att lära dig mer om hur du jämför datum i Fish Shell:
- [Fish Shell dokumentation](https://fishshell.com/docs/current/cmds/date.html)
- [Jämföra tidsintervall i Fish Shell](https://fishshell.com/docs/current/cmds/seq.html#comparing-time-intervals)
- [Beräkna skillnaden mellan två datum i Fish Shell](https://askubuntu.com/questions/921529/how-do-i-calculate-the-difference-between-two-dates)