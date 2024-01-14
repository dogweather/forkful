---
title:                "Elixir: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa tillfälliga filer är en vanlig uppgift inom programmering. Det kan vara användbart för att hantera data som bara behövs för en kort period av tid, eller för att skapa temporära backup-filer för större projekt. I denna bloggartikel kommer vi att ta en titt på hur vi kan skapa tillfälliga filer med Elixir.

## Så här gör du

För att skapa en tillfällig fil i Elixir kan vi använda oss av funktionen `{:ok, path} = Tempfile.open()`. Detta kommer att skapa en fil med ett slumpmässigt namn i operativsystemets standardtemporära filkatalog.

```Elixir
{:ok, path} = Tempfile.open()
IO.puts("Tillfällig fil skapad: #{path}")
```
Output: Tillfällig fil skapad: /tmp/138b542e4a

Om vi vill välja en annan temporär katalog kan vi istället ange sökvägen som ett argument till `Tempfile.open()`:

```Elixir
{:ok, path} = Tempfile.open("/var/tmp")
IO.puts("Tillfällig fil skapad: #{path}")
```
Output: Tillfällig fil skapad: /var/tmp/840f45c3a6

En annan användbar funktion när det gäller tillfälliga filer är `Tempfile.directory()`, som ger oss sökvägen till den aktuella standardtemporära katalogen:

```Elixir
path = Tempfile.directory()
IO.puts("Standardtemporär katalog: #{path}")
```
Output: Standardtemporär katalog: /tmp

Nu när vi vet hur vi skapar tillfälliga filer, låt oss dyka lite djupare och titta på vad som händer bakom kulisserna.

## Djupgående

När vi använder funktionen `Tempfile.open()` skapar Elixir ett objekt som representerar den tillfälliga filen. Detta objekt innehåller information som sökvägen till filen, dess storlek och om filen har blivit raderad eller inte. Om vi vill kan vi utforska dessa attribut genom att använda oss av `Kernel.inspect()`:

```Elixir
{:ok, tempfile} = Tempfile.open()
IO.puts("Intern sökväg: #{inspect(tempfile.path)}")
IO.puts("Storlek: #{inspect(tempfile.size)}")
IO.puts("Raderad: #{inspect(tempfile.deleted?)}")
```
Output:
Intern sökväg: "/tmp/eb52ebe8c7"
Storlek: 0
Raderad: false

En annan intressant funktion som finns tillgänglig är `Tempfile.close()`, som stänger den tillfälliga filen och raderar den från filsystemet. Om vi däremot vill behålla den tillfälliga filen, kan vi använda `Tempfile.unlink()`, som enbart tar bort filen utan att stänga den:

```Elixir
{:ok, tempfile} = Tempfile.open()
IO.puts("Tillfällig fil skapad i sökväg: #{tempfile.path}")
Tempfile.unlink(tempfile)
IO.puts("Tillfällig fil borttagen från sökväg: #{tempfile.path}")
```
Output:
Tillfällig fil skapad i sökväg: /tmp/d7c51b5336
Tillfällig fil borttagen från sökväg: /tmp/d7c51b5336

Nu när vi har utforskat hur vi kan skapa och hantera tillfälliga filer med Elixir, kan du använda denna kunskap nästa gång du behöver hantera temporära data inom dina projekt.

## Se även

- [Elixir - Tempfile](https://hexdocs.pm/elixir/Tempfile.html)
- [Gigalixir - Temporary File Handling in Elixir](https://gigalixir.readthedocs.io/en/latest/tmpfile.html)
- [ElixirSchool - Files](https://elixirschool.com/en/lessons/basics/files/)