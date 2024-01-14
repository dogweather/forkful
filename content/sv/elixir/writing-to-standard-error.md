---
title:    "Elixir: Skriva till standardfel"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error, eller stderr, är ett sätt att skicka ut felmeddelanden och andra viktiga meddelanden som är avsedda för programmeraren att se. Genom att använda detta verktyg kan du få värdefull information om din kod och identifiera eventuella problem.

## Hur man gör det

För att skriva till stderr i Elixir, kan du använda funktionen `IO.puts/2` med `stderr` som det andra argumentet. Här är ett exempel på hur du skulle använda det:

```Elixir
IO.puts("Detta är en viktig varning!", stderr)
```

Detta kommer att skriva ut meddelandet till standard error istället för standard output. Det är också möjligt att använda `IO.puts/1` och bara skicka meddelandet till stderr som standard utan att behöva ange det som ett argument.

## Djupdykning

När du skriver till stderr i Elixir, är det viktigt att notera att meddelandet också kommer att skickas till standard output. Detta kan vara användbart om du vill se meddelandet i terminalen, men om du bara vill ha det i stderr kan du använda `IO.puts/2` för att skriva till standard error endast.

En annan viktig sak att komma ihåg är att stderr inte buffras. Det betyder att meddelandena skrivs ut direkt utan några fördröjningar. Detta gör stderr särskilt användbart när du felsöker din kod och behöver snabb återkoppling.

## Se även

- [Elixir Dokumentation: IO.puts/2](https://hexdocs.pm/elixir/IO.html#puts/2)
- [Elixir Dokumentation: IO.puts/1](https://hexdocs.pm/elixir/IO.html#puts/1)
- [Elixir Dokumentation: stderr](https://hexdocs.pm/elixir/Kernel.html#stderr/0)