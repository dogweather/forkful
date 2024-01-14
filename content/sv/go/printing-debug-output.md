---
title:    "Go: Utskrift av felsökningsutdata"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

Att skriva kod är en process som ofta går hand i hand med att hitta och åtgärda fel. Ibland kan det vara svårt att förstå vad som egentligen händer i koden när ett fel uppstår. Här kommer printing av debug output till undsättning. Genom att skriva ut information om variabler och steg i processen kan du enklare förstå vad som händer i din kod och därmed effektivare lösa problemen.

## Hur man gör

För att skriva ut debug output i Go kan du använda dig av funktionen `fmt.Println()`. Låt oss säga att du har en variabel `x` som innehåller värdet 5 och du vill skriva ut värdet för att kontrollera att det är korrekt. I så fall kan du skriva följande kod:

```Go
x := 5
fmt.Println(x)
```

I konsolen kommer då följande output att visas:

```
5
```

Du kan även printa ut flera variabler och kombinera olika datatyper. Till exempel:

```Go
a := "Hej"
b := 10
fmt.Println(a, "världen!", b)
```

Detta skulle ge output:

```
Hej världen! 10
```

## Djupdykning

Utöver att bara printa ut variabler kan du även använda dig av printfunktioner för att fördjupa ditt debuggande. Med `fmt.Printf()` kan du specificera formatet på det du vill printa, till exempel vad det ska vara för datatyp. Du kan också använda `%v` för att printa ut en variabels värde och `%T` för att printa ut datatypen.

Exempel:

```Go
x := 5
fmt.Printf("Värdet på x är %v och datatypen är %T", x, x)
```

Output:

```
Värdet på x är 5 och datatypen är int
```

## Se även

- [Officiell Go dokumentation för fmt paketet](https://golang.org/pkg/fmt/)
- [Debugging i Go: Bra praktiker och verktyg](https://blog.golang.org/debugging-go-code)
- [En guide till debugging med printf](https://www.calhoun.io/intro-to-go-interfaces-debugging-with-printf/)