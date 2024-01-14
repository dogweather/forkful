---
title:                "Bash: Läsa kommandoradsargument"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
I dagens digitala värld är kunskap om terminalkommandon viktigare än någonsin. Genom att lära sig att läsa kommandoradsargument kan du effektivisera ditt arbete och få ännu mer kontroll över din dator. Det gör det också möjligt att skriva skräddarsydda skript för att automatisera uppgifter.

## Så här gör du
Att läsa kommandoradsargument i Bash är enkelt, det handlar bara om att känna till rätt syntax. Här är ett exempel på enkel syntax för att läsa in ett argument och skriva ut det i terminalen:

```Bash 
echo "Det första argumentet är: $1"
```

Om du kör detta skript med `sh script.sh test`, kommer det att skriva ut `Det första argumentet är: test`.

För att läsa flera argument kan du använda dig av en `for`-loop och `$@` som representerar alla inmatade argument. Här är ett exempel:

```Bash 
for argument in "$@"
do
	echo "Argumentet är: $argument"
done
```

Om du kör detta skript med `sh script.sh test1 test2 test3`, kommer det att skriva ut:

```
Argumentet är: test1
Argumentet är: test2
Argumentet är: test3
```

## Djupdykning
Genom att känna till rätt syntax, kan du läsa och hantera kommandoradsargument på ett smidigt sätt. Här är några viktiga saker att tänka på:

- `$0` representerar själva skriptet och `$1`, `$2` osv. representerar inmatade argument.
- Du kan använda villkorsuttryck som `if`-satser för att hantera olika scenarier baserat på de inmatade argumenten.
- Använd `shift`-kommandot för att flytta fram positionen på argumenten om du vill hoppa över det första eller flera argument.

## Se även
- [Bash: Command Line Arguments](https://www.tutorialspoint.com/unix/shell_scripting.htm)
- [Reading Command Line Arguments in Bash](https://linuxconfig.org/learning-linux-bash-scripting-syntax-reading-command-line-arguments)
- [Using Bash Scripts to Automate Tasks](https://linuxhint.com/bash_script_task_automation/)