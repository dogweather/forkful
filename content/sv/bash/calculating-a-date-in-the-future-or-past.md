---
title:    "Bash: Beräkna ett datum i framtiden eller det förflutna"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför
Det kan finnas många olika anledningar till varför man skulle vilja räkna ut ett datum i framtiden eller i det förflutna. Det kan vara för att planera ett event eller en resa, hålla koll på viktiga deadlines eller bara för att det är intressant att veta.

## Så här gör du
Att räkna ut ett datum i framtiden eller förflutna är faktiskt enkelt med hjälp av Bash programmering. Detta är ett praktiskt verktyg som finns tillgängligt på de flesta Linux-datorer och är perfekt för att automatisera uppgifter och beräkningar.

För att beräkna ett datum i framtiden använder vi oss av kommandot `date` och dess flagga `--date`. Till exempel, om vi vill beräkna 100 dagar framåt kan vi skriva följande i terminalen:

```Bash
date --date="100 days"
```

Du kommer då få ett datum som ligger 100 dagar framåt i tiden, och utdata kommer se ut något liknande detta:

```
tis 23 mar 2021 00:00:00 CET
```

För att beräkna ett datum i förflutna kan vi använda en liknande syntax. Till exempel, om vi vill veta vilket datum det var för 10 år sedan skriver vi:

```Bash
date --date="10 years ago"
```

Detta ger oss utdatan:

```
mån 23 mar 2009 00:00:00 CET
```

Det är även möjligt att använda specifika datum eller tidsintervall, såsom "next Monday", "last month", eller till och med en viss tid på dagen, till exempel "1pm".

## Djupdykning
För de som är intresserade av att lära sig mer detaljerad information om att beräkna datum i Bash, finns det gott om resurser att utforska. Ett viktigt koncept att förstå är hur `date` kommandot hanterar tidszoner och vilken standard som används. Det finns även möjlighet att ange egna tidszoner och datumformat för ännu mer precision.

En annan viktig faktor är att kommandot endast kan hantera datum från år 1902 och framåt. För datum före detta kan man använda sig av kommandot `cal` för att få information om en specifik månad eller år.

## Se även
Här är några användbara länkar för de som är intresserade av att lära sig mer om att beräkna datum i Bash:

- [Bash Documentation](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- [Documentation för `date` kommandot](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/html_node/Bash-Conditional-Expressions.html#Bash-Conditional-Expressions)
- [Resurser för att lära sig mer om Bash programmering](https://www.learnshell.org/sv/)