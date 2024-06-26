---
date: 2024-01-20 17:36:00.270443-07:00
description: "S\xE5 G\xF6r Du: Bash anv\xE4nder `date`-kommandot f\xF6r att omvandla\
  \ datum till str\xE4ngar. H\xE4r \xE4r n\xE5gra exempel."
lastmod: '2024-03-13T22:44:38.093714-06:00'
model: gpt-4-1106-preview
summary: "Bash anv\xE4nder `date`-kommandot f\xF6r att omvandla datum till str\xE4\
  ngar."
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

## Så Gör Du:
Bash använder `date`-kommandot för att omvandla datum till strängar. Här är några exempel:

```Bash
# Aktuellt datum och tid som sträng
date
```

```Bash
# Anpassa formatet till ÅÅÅÅ-MM-DD
date +"%Y-%m-%d"
```

```Bash
# Skapa en sträng för första dagen i nästa månad
date +"%Y-%m-01" -d "next month"
```

Sample output:

```
tors 9 mar 14:49:23 CET 2023
2023-03-09
2023-04-01
```

## Fördjupning
`date`-kommandot i Unix-liknande system som Linux har existerat sedan 70-talet, ursprungligen utvecklat i AT&T's Bell Laboratories. Det finns olika alternativ för att omvandla datum till strängar, som att använda externa program som `awk` eller `perl`. Implementationen i Bash sker normalt genom inbyggda funktioner (`date`), men man kan också kalla externa tjänster eller API:er för mer avancerade behov.

## Se Även
- GNU Coreutils documentation for 'date': https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/dates.html
- Stack Overflow, for troubleshooting: https://stackoverflow.com/questions/tagged/date+bash
