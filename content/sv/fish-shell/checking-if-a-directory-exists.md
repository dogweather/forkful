---
title:    "Fish Shell: Kontrollera om en mapp finns"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför

Ibland när du gör din Fish Shell-script, kanske du behöver kontrollera om en mapp finns innan du fortsätter med resten av koden. Att veta om en mapp finns kan hjälpa till att förhindra fel och göra koden mer robust.

## Så här gör man

För att kontrollera om en mapp finns i Fish Shell, kan du använda kommandot "test -d". Detta kommer att returnera true om mappen finns och false om den inte gör det. Här är ett exempel på hur du kan använda detta kommando:

```Fish Shell
if test -d "mappnamn"
  echo "Mappen finns!"
else
  echo "Mappen finns inte."
end
```

I detta exempel använder vi "if" och "else" för att skriva ut ett meddelande beroende på resultatet av "test -d" -kommandot.

## Djupdykning

Det finns flera olika sätt att kontrollera om en mapp finns i Fish Shell. En annan metod är att använda kommandot "contains" tillsammans med "test -d". Här är ett exempel på detta:

```Fish Shell
if contains "mappnamn" (ls)
  echo "Mappen finns!"
else
  echo "Mappen finns inte."
end
```

I detta exempel använder vi "ls" för att lista alla filer och mappar i nuvarande mapp, och sedan kollar vi om "mappnamn" finns med hjälp av "contains". Om den finns kommer vi att skriva ut "Mappen finns!".

Det finns också andra kommandon som du kan använda för att kontrollera om en mapp finns, till exempel "status" och "find". Utforska gärna dessa för att hitta det som fungerar bäst för ditt specifika användningsfall.

## Se även

- [Fish Shell dokumentation för kommandot "test"](https://fishshell.com/docs/current/cmds/test.html)
- [En guide för Fish Shell-programmering](https://fishshell.com/docs/current/index.html)
- [En fullständig lista över Fish Shell-kommandon](https://fishshell.com/docs/current/commands.html)