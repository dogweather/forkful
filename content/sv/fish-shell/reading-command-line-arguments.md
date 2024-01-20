---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Kommandoradsargument är indata som ges till ett skript vid exekvering. Programmerare användar detta för att styra skriptets beteende utan att ändra själva koden.

## Så här gör du:
Med Fish Shell är det superenkel att läsa kommandoradsargument med speciellt variabeln argv. Här är det grundläggande mönstret:

```Fish Shell
#!/usr/bin/env fish
for arg in $argv
    echo $arg
end
```

Kör skriptet med några argument:

```Fish Shell
> ./myscript.fish ett två tre
```

Det ger följande utdata:

```Fish Shell
ett
två
tre
```

## Fördjupning
Kommandoradsargument har använts i Unix-baserade system sedan 1970-talet. Det finns andra sätt att ge indata till ett skript, till exempel via standard input eller konfigurationsfiler, men kommandoradsargument används fortfarande eftersom det är enkelt och flexibelt. I Fish Shell, lagras argumenten i en lista av strängar i $argv som kan indexeras och skivna, liknande att arbeta med en lista i Python.

## Se även
För ytterligare läsning och exempel på hur du arbetar med kommandoradsargument i Fish Shell, se följande:
- [Fish docs: Commandline arguments](https://fishshell.com/docs/current/tutorial.html#tut_commandline_args)
- [StackOverflow: How to handle command-line arguments in Fish](https://stackoverflow.com/questions/7306962/fish-shell-how-to-set-multi-word-variable-from-argv)
- [GitHub Fish Script examples](https://github.com/jorgebucaran/fish-shell-cookbook#commandline-arguments)