---
title:                "Fish Shell: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin stött på situationer där du behöver skapa en temporär fil när du använder Fish Shell? Det finns faktiskt flera användbara fall där en temporär fil kan hjälpa dig att hantera dina uppgifter bättre. Läs vidare för att upptäcka varför du kanske vill skapa en temporär fil och hur du kan göra det i Fish Shell.

## Hur du gör

Att skapa en temporär fil i Fish Shell är ganska enkelt. Du kan använda kommandot `mktemp` för att skapa en temporär fil och tilldela den ett unikt namn. Till exempel:

```Fish Shell
mktemp
```
Output: `/tmp/tmp.VuqMXgUHh5`

Som du kan se i exemplet skapas en fil i `/tmp` mappen och tilldelar den ett unikt namn bestående av bokstäver och siffror. Du kan också ange ett prefix för filnamnet genom att använda flaggan `-p`, till exempel:

```Fish Shell
mktemp -p foo
```
Output: `/tmp/foo.HmTwZ3aqrK`

Detta lägger till prefixet "foo" till det unika namnet på temporärfilen. Du kan också välja var temporärfilen ska skapas genom att använda flaggan `-t`, till exempel:

```Fish Shell
mktemp -t ~/Desktop
```
Output: `/Users/username/Desktop/tmp.OgPTJ6uM13`

Nu skapas temporärfilen direkt på skrivbordet istället för i /tmp mappen.

## Djupdykning

Kommandot `mktemp` används för att skapa temporära filer som används för tillfälliga uppgifter eller för att undvika att skriva över befintliga filer. Detta är särskilt användbart när du skapar skript eller automatiseringsverktyg som använder sig av tillfälliga filer för att hantera data.

Standardmappen för temporära filer är `/tmp` men det är möjligt att ändra detta genom att redigera variabeln `TMP` i Fish Shell. Du kan också kontrollera var temporärfiler skapas genom att använda flaggan `-u`.

Som standard skapas temporära filer med läs- och skrivrättigheter för den aktuella användaren. Men om du vill ange specifika rättigheter kan du använda `-m` flaggan och ange rättigheter i octal format, till exempel:

```Fish Shell
mktemp -m 700
```
Output: `/tmp/foo.iN1m1JYR64`

Här tilldelas temporärfilen rättigheter 700, vilket innebär att endast den aktuella användaren har rättigheter att läsa och skriva i filen.

## Se även

- [Fish Shell dokumentation för mktemp](https://fishshell.com/docs/current/commands.html#mktemp)
- [Linux-kommandot för mktemp](https://linux.die.net/man/1/mktemp)
- [Bash-script: Använda temporära filer](https://linuxize.com/post/bash-temporary-files/)