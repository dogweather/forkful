---
title:                "Läsning av kommandoradsargument"
html_title:           "Fish Shell: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Det finns många olika sätt att interagera med datorer, men ibland kan det enklaste sättet vara via kommandoraden. Genom att läsa kommandoradsargument kan du snabbt och enkelt utföra olika uppgifter utan att behöva öppna ett grafiskt gränssnitt. I denna artikel kommer vi att utforska hur du kan läsa kommandoradsargument i Fish Shell och hur det kan förenkla din arbetsflöde.

## Så här

### Skapa ett nytt Fish Shell-skript
Först måste du skapa ett nytt skript i Fish Shell. Öppna din terminal och skriv in kommandot `touch script.fish` för att skapa en ny tom fil med namnet "script". Du kan sedan öppna filen i din favorittextredigerare för att börja koda.

```Fish Shell
touch script.fish // Skapar en ny fil med namnet script
```

### Läs ett specifikt kommandoradsargument
För att läsa ett specifikt kommandoradsargument kan du använda variabeln `$argv`. Den innehåller en lista med alla argument som skickas till ditt skript. I exemplet nedan kommer vi att läsa första argumentet som skickas till skriptet och skriva ut det i terminalen.

```Fish Shell
echo $argv[1] // Skriver ut första argumentet
```

Om vi nu kör vårt skript med kommandot `./script.fish Hello`, kommer output att bli "Hello" eftersom "Hello" är det första argumentet som vi skickade till skriptet.

### Loopa igenom alla kommandoradsargument
Ibland kan du vilja läsa och hantera flera kommandoradsargument. För att göra det kan du använda en for-loop för att loopa igenom alla argumenten. I exemplet nedan kommer vi att skriva ut varje argument i en egen rad.

```Fish Shell
for arg in $argv
  echo $arg
end
```

Kör du nu skriptet med kommandot `./script.fish Hello World`, kommer output att bli:

```
Hello
World
```

## Djupdykning

Det finns många olika kommandoradsalternativ som du kan använda för att läsa argument i Fish Shell. Här är några exempel:

- `$argv[0]` innehåller namnet på det skript som körs.
- Om ingen input ges i kommandoraden, kommer `$argv` att vara en tom lista.
- `$argv[-1]` innehåller det sista argumentet i listan.
- Du kan även använda `$#` för att få antalet argument som skickats till skriptet.

## Se också

- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
- [En introduktion till Fish Shell](https://dev.to/jonmcalder/an-introduction-to-fish-shell-3b7)
- [Bash vs Fish Shell: En jämförelse](https://medium.com/@ivanaugustobd/bash-vs-fish-shell-e7080a184557)