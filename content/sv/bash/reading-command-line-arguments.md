---
title:                "Bash: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Många program behöver ta emot input från användaren för att utföra sina uppgifter. Det kan handla om en söksträng för en sökmotor, ett förnamn och efternamn för en registrering eller ett filnamn för att öppna en fil. Genom att kunna läsa och använda kommandoradsargument kan du skräddarsy dina program för att göra dem mer användarvänliga och kraftfulla.

## Hur man läser kommandoradsargument

Det finns flera sätt att läsa kommandoradsargument i Bash. Ett sätt är att använda variabeln $1 för att hämta det första argumentet, $2 för det andra argumentet och så vidare. Du kan också använda $0 för att hämta hela kommandoraden. Nedan är ett exempel på hur du kan använda dessa variabler:

```Bash
# Hämta det första argumentet och skriv ut det
echo "Det första argumentet är: $1"
# Hämta det andra argumentet och använda det för en sökning
grep "$2" file.txt
# Hämta hela kommandoraden och skriv ut den
echo "Kommandoraden är: $0"
```

**Output:**
```Bash
$ bash command_line_args.sh hello world
Det första argumentet är: hello
Kommandoraden är: bash command_line_args.sh hello world
```
Det är viktigt att notera att argumenten ska anges efter filnamnet när du kör skriptet i terminalen.

## Djupdykning

Utöver att använda variabler kan du även använda read-kommandot för att läsa kommandoradsargument. Detta gör det möjligt att läsa flera argument på en rad och till och med be användaren om input om argument inte har angetts. Nedan är ett exempel på hur du kan använda read för att läsa tre argument och sedan skriva ut dem:

```Bash
read arg1 arg2 arg3
echo "Argument 1 är: $arg1"
echo "Argument 2 är: $arg2"
echo "Argument 3 är: $arg3"
```

**Output:**
```Bash
$ bash read_args.sh hej kompis!
Argument 1 är: hej
Argument 2 är: kompis!
Argument 3 är:
```

Om du vill kan du också specificera en prompt för användaren att svara på, vilket kan göra det tydligare vad programmet förväntar sig som input:

```Bash
read -p "Ange ditt för- och efternamn: " first_name last_name
echo "Välkommen $first_name $last_name!"
```

**Output:**
```Bash
$ bash read_args.sh
Ange ditt för- och efternamn: John Doe
Välkommen John Doe!
```
Det finns också möjlighet att använda flaggor för att läsa specifika argument. Flaggor kan definieras med "-". Till exempel om du vill läsa ett valfritt argument kan du använda flaggan "-f" och sedan ange en standardvärde om argumentet inte ges. Nedan är ett exempel:

```Bash
# Standardvärdet för argumentet är "världen"
while getopts ":f" option; do
	case $option in
		f) arg1=$OPTARG ;;
	esac
done
echo "Hej $arg1!"
```

**Output:**
```Bash
$ bash flag_args.sh
Hej världen!
$ bash flag_args.sh -f "Sverige"
Hej Sverige!
```

## Se även

- [Bash dokumentation](https://www.gnu.org/software/bash/manual/bash.html#Shell-Variables)
- [Tutorial: bash kommandoradsargument](https://www.tutorialspoint.com/unix_commands/bash_shell_scripting.htm)