---
title:    "Bash: Omvandla en datum till en sträng"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför

Att konvertera en datum till en sträng är en vanlig uppgift inom Bash-programmering. Det kan behövas för att formatera datumet på ett visst sätt för att användas i en filnamn, loggfil eller annan rapportering.

## Hur man gör det

Det finns flera olika sätt att konvertera ett datum till en sträng inom Bash. Här är ett exempel på hur du kan göra det med hjälp av kommandot `date` och `printf`:

```Bash
datum=$(date +"%Y-%m-%d") # Hämtar aktuellt datum och lagrar det i variabeln datum.
printf "Dagens datum är %s" "$datum" # Skriver ut värdet av variabeln datum till konsolen.
```

Output:
```Bash
Dagens datum är 2021-05-20
```

Det finns också möjlighet att använda `strftime` kommandot för att formatera datumet på ett visst sätt. Här är ett exempel på hur du kan använda det för att få fram den aktuella veckodagen:

```Bash
dag=$(strftime "%A" $(date +"%s")) # Hämtar veckodagen från det aktuella datumet och lagrar det i variabeln dag.
printf "Idag är det %s" "$dag" # Skriver ut värdet av variabeln dag till konsolen.
```

Output:
```Bash
Idag är det torsdag
```

## Djupdykning

När vi konverterar ett datum till en sträng, kan vi också inkludera tidsformatet. Detta kan göras genom att använda `%H:%M:%S` för att få ut timmar, minuter och sekunder i 24-timmars format. Om vi istället vill ha tiden i 12-timmars format med AM/PM-indikatorer kan vi använda `%I:%M:%S%p`.

Att konvertera ett datum till en sträng är också en viktig del av automatiserade uppgifter inom Bash-programmering. Genom att skapa ett skript som konverterar och inkluderar datumet i en fil eller loggfil, kan vi enkelt spåra när en uppgift utfördes.

## Se även

- [Bash-programmering för nybörjare](https://www.linuxjournal.com/content/bash-programming-beginners)
- [Bash-kommandon för att hantera datum och tid](https://linuxize.com/post/bash-current-date-time/)
- [Dokumentation för `date` och `strftime` kommandon](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)