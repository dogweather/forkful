---
title:    "Elixir: Utskrift av avlusningsutdata"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att skriva kod är ofta en rolig och spännande upplevelse. Dock kan det ibland bli en utmaning att felsöka när programmet inte fungerar som det ska. Det är här utskrift av felsökningsinformation, eller "debug output" på engelska, kan vara till stor hjälp. Genom att skriva ut värden, variabler och steg i koden kan du få en bättre förståelse för vad som händer och var problemet ligger.

## Hur man gör

För att skriva ut debug output i Elixir används funktionen `IO.inspect/2`. Denna funktion tar som input ett värde eller en variabel och skriver ut dess värde till konsolen. Här är ett enkelt exempel på hur du kan använda `IO.inspect/2`:

```Elixir
x = 5
IO.inspect(x)
```

Detta kommer att skriva ut värdet av variabeln `x` som är 5. Om du har flera värden du vill skriva ut kan du använda en lista som input till funktionen:

```Elixir
list = [1, 2, 3]
IO.inspect(list)
```

Detta kommer att skriva ut hela listan med dess element.

Du kan också använda `IO.inspect/1` för att skriva ut ett helt uttryck, som i detta exempel:

```Elixir
IO.inspect(2 + 3)
```

Detta kommer att skriva ut värdet av uttrycket, vilket i detta fall är 5.

## Djupdykning

Det finns flera olika användbara inmatningsparametrar för `IO.inspect/2` som kan skräddarsy utskriftens utseende. Du kan till exempel ange en `label` som ett andra argument för att tydligare identifiera vad som skrivs ut. Här är ett exempel:

```Elixir
x = 5
IO.inspect(x, label: "Value of x")
```

Detta kommer att skriva ut "Value of x: 5".

En annan användbar inmatningsparameter är `depth`, som låter dig specificera hur djupt du vill att värdet ska inspekteras. Om du till exempel har en komplex datastruktur som en lista med flera inbäddade listor, kan du använda `depth` för att ange hur många steg av inbäddning du vill att funktionen ska inspektera.

För mer information om olika inmatningsparametrar och hur du kan använda dem, se Elixirs dokumentation för `IO.inspect/2`.

## Se också

- [Elixirs officiella dokumentation om IO.inspect/2](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [En guide för att skriva ut debug output i Elixir](https://www.cultivatehq.com/posts/elixir-debugging-with-inspect/)
- [En video tutorial om användning av IO.inspect i Elixir](https://www.youtube.com/watch?v=dfGyL_hKSxE)