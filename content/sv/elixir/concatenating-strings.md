---
title:    "Elixir: Sammanfogning av strängar"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar (concatenation) är en vanlig uppgift vid programmering. Det kan användas för att skapa dynamisk text, formulär eller presentera data på ett lättbegripligt sätt. I Elixir är det väldigt enkelt att sammanslå strängar och i denna bloggpost kommer vi att titta närmare på hur man gör det.

## Hur man gör

I Elixir finns det ett antal olika sätt att sammanslå strängar på. Det enklaste sättet är att använda operatorn `<>`. Detta operatorn tar två strängar och sätter ihop dem till en enda sträng.

```Elixir
"Hello" <> " world!" 
```

Detta kommer att ge oss resultatet `Hello world!`.

En annan metod är att använda Elixirs modul `String`. Denna modul har en funktion `concat` som tar in en lista av strängar och sätter ihop dem.

```Elixir
String.concat(["Hello", " ", "world!"])
```

Detta kommer också att ge oss samma resultat `Hello world!`.

Vi kan också använda interpolering genom att sätta in variabler direkt i strängen med `#{}`.

```Elixir
name = "John"
"Hello, #{name}!"
```

Detta kommer att ge oss `Hello, John!`.

En annan viktig del av att sammanslå strängar är att hantera specialtecken och formattering. I Elixir kan vi använda `|>` (pipeline operator) tillsammans med `String.replace` för att hantera detta.

```Elixir
"Hello, world!" |> String.replace("o", "0")
```

Detta kommer att ge oss resultatet `Hell0, w0rld!`.

## Djupdykning

Det är viktigt att notera att sammanslående strängar i Elixir skapar en ny kopia av strängen varje gång. Detta innebär att om en sträng behöver sammanslås många gånger, kan det vara mer effektivt att använda `IOLists` i Elixir som minimerar antalet kopior som skapas.

För att undvika problem med minnesallokering kan det också vara användbart att använda `<<>>` syntax istället för `<>`.

## Se också

- [Elixir String Modul](https://hexdocs.pm/elixir/String.html)
- [Elixir IOLists](https://elixirschool.com/sv/lessons/advanced/io-lists/)
- [Elixir Pattern Matching](http://elixir-lang.org/getting-started/pattern-matching.html)