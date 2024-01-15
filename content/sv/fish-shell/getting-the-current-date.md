---
title:                "Att få aktuellt datum"
html_title:           "Fish Shell: Att få aktuellt datum"
simple_title:         "Att få aktuellt datum"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta den aktuella datumet är en användbar funktion när du vill organisera, sortera eller märka filer på din dator. Och i Fish Shell är det enkelt att få den aktuella datumet med några enkla kommandon.

## Så här gör du

För att få den aktuella datumet i Fish Shell, behöver du använda funktionen `date` tillsammans med flaggan `-I` som specificerar formatet på datumet. Här är en kodexempel:

```
fish shell> date -I
2021-09-24
```

Som du kan se returnerar kommandot datumet i formatet ÅÅÅÅ-MM-DD. Men du kan också lägga till flaggan `-u` för att få datumet i UTC-tid istället för din lokala tidszon.

```
fish shell> date -I -u
2021-09-24
```

Om du bara vill ha en del av datumet, som t.ex. bara månaden eller bara året, kan du använda flaggan `-d` tillsammans med `date`. Här är några exempel:

```
fish shell> date -d "next month"
2021-10-24
fish shell> date -d "2 years ago"
2019-09-24
fish shell> date -d "today + 5 days"
2021-09-29
```

Du kan också använda flaggan `-r` för att få datumet från en specifik fil. Till exempel om du vill få datumet för senaste ändringen av en fil:

```
fish shell> date -r file.txt
2021-09-23
```

## Djupdykning

Fish Shell använder funktionen `date` från operativsystemet för att få den aktuella datumet, så resultatet kommer att skilja sig beroende på vilket operativsystem du använder. Det finns också många fler flaggor och alternativ som du kan använda med `date` för att få tidsstämplar, tidszoner och mer.

En annan intressant funktion är att du kan skapa en variabel för att spara datumet och sedan använda den i andra kommandon. Till exempel:

```
fish shell> set today (date -I)
fish shell> echo $today
2021-09-24
fish shell> touch $today-file.txt
```

När du skapar en fil med namnet `2021-09-24-file.txt` kommer den att ha den aktuella datumet som namn. Du kan också använda variabeln `today` i andra kommandon eller skript för att organisera filer eller märka anteckningar.

## Se även

- [Fish Shell dokumentation om kommandot "date"](https://fishshell.com/docs/3.3/cmds/date.html)
- [En guide för att använda variabler i Fish Shell](https://jvns.ca/blog/2017/04/02/the-unix-philosophy--pai-/)