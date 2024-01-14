---
title:                "Elixir: Att skriva en textfil"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil med Elixir kan vara ett användbart verktyg för att lagra och manipulera data på ett strukturerat sätt. Det kan också vara ett sätt att öva och förbättra sina programeringsfärdigheter inom Elixir.

## Så här gör du

För att skriva en textfil med Elixir behöver du först öppna en fil med önskat namn och filformat, t.ex. "mittexempel.txt". Sedan kan du använda funktionen "File.write" för att skriva innehållet i filen.

```Elixir
iex> fil = File.open("mittexempel.txt", [:write])
#File<file mittexempel.txt>
iex> File.write(fil, "Hej världen!")
:ok
```

För att läsa innehållet i filen kan du använda funktionen "File.read":

```Elixir
iex> fil = File.open("mittexempel.txt")
#File<file mittexempel.txt>
iex> File.read(fil, :all)
"Hej världen!"
```

Du kan också använda "File.close" för att stänga filen när du är klar med den:

```Elixir
iex> File.close(fil)
:ok
```

## Djupdykning

När du skriver en textfil med Elixir har du möjlighet att lägga till olika options för att anpassa hur filen ska skrivas. Till exempel kan du ange om filen ska skrivas över en befintlig fil, om ett nytt innehåll ska läggas till slutet av filen eller om filen ska skrivas som binärt.

För att skriva över en befintlig fil, sätt options till :write och :overwrite:

```Elixir
iex> File.write(fil, "Hej världen!", [:write, :overwrite])
:ok
```

För att lägga till innehåll längst till slutet av filen, sätt options till :append:

```Elixir
iex> File.write(fil, "Ännu en rad", [:write, :append])
:ok
```

Och om du vill skriva filen som binärt, använd options :binary:

```Elixir
iex> File.write(fil, "Binärt innehåll", [:write, :binary])
:ok
```

Se Also

- [Elixir Dokumentation om "File"](https://hexdocs.pm/elixir/File.html)
- [En guide till Elixir för nybörjare](https://learnxinyminutes.com/docs/elixir/)
- [En lista över populära Elixir-projekt](https://github.com/h4cc/awesome-elixir)