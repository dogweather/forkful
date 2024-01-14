---
title:    "Fish Shell: Beräkna ett datum i framtiden eller i det förflutna"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

### Varför

Att kunna beräkna datum i framtiden eller det förflutna är en användbar funktion för många programmerare. Det kan vara användbart för att planera scheman, skapa påminnelser eller hålla koll på deadlines.

### Hur man gör det

Det finns flera sätt att beräkna datum i Fish Shell, men ett enkelt sätt är att använda kommandot `date`. För att få ett datum till exempelvis fem dagar framåt i tiden kan man använda följande kod:

```
Fish Shell  -c 'date -v + 5d'
```

Detta kommer att ge ett datum som är fem dagar efter det aktuella datumet. Om man istället vill ha ett datum fem dagar innan det aktuella datumet, kan man använda `-v-5d`.

### Djupdykning

Fish Shell erbjuder många olika alternativ för att beräkna datum i framtiden eller det förflutna. Det finns möjlighet att använda olika tidsintervall som dagar, veckor, månader eller år. Man kan också använda sig av `add` eller `subtract` istället för `date -v` för att få ännu mer anpassningsbara datum.

En annan användbar funktion är möjligheten att få datumet för en specifik dag i veckan. Till exempel, om man vill ha datumet för nästa fredag, kan man skriva:

```
Fish Shell -c 'date -vnext friday'
```

Detta kommer att ge datumet för nästa fredag. Det finns också möjlighet att få datumet för tidigare dagar i veckan, till exempel `last friday`.

### Se även

Här är några användbara resurser för att lära sig mer om beräkning av datum i Fish Shell:

- [Fish Shell dokumentation] (https://fishshell.com/docs/current/cmds/date.html)
- [Fish Shell tutorial] (https://www.linux.com/training-tutorials/learn-fish-shell/)
- [Fish Shell Community] (https://github.com/fish-shell/fish-shell)