---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skapa en temporär fil är ett sätt att lagra data tillfälligt under programmets körning. Programmerare gör detta när de behöver bearbeta stora mängder data, hålla dem i minnet skulle vara opålitligt eller kostsamt.

## Hur gör man:
Gleam har inte inbyggd stöd för att skapa temporära filer, men vi kan använda Erlang's `:file.mktemp` funktion, som Gleam kan kalla:

```gleam
import erlang

fn erlang_temp_file(prefix: String, suffix: String) -> Result(String, Nil) {
  erlang.apply(:file, :mktemp, [prefix, suffix])
  |> Result.from_erlang
}

fn main(args: List(String)) {
  case erlang_temp_file("prefix_", ".suffix") {
    Ok((path, _fd)) ->
      erlang.display(path)

    Error(Nil) ->
      erlang.display("Unable to create a temp file.")
  }
}
```

När du kör detta program, kommer du att se en utdata som liknar:

```
/tmp/prefix_xxxxxx.suffix
```

## Djupdykning
Att skapa temporära filer är en gammal koncept och har använts i datavetenskap i decennier för att hantera minnesproblem. Det finns alternativ, som att använda databaser eller distribuerade cache-system som Redis. Temporära filer är emellertid enkla att implementera och kräver ingen ytterligare setup. Intern implementation av metoden `:file.mktemp` genererar en unik filnamn som inte redan finns i systemet.

## Se även
Kolla in Erlang's officiella dokumentation om att skapa temporära filer:
- [`:file.mktemp`](http://erlang.org/doc/man/file.html#mktemp-2)
Råd om när och hur man ska använda temporära filer:
- [What are temporary files and how to use them](https://www.gnu.org/software/autogen/mktemp.html)
- [When should I use a temporary file](https://dba.stackexchange.com/questions/11114/when-should-i-use-a-temporary-table-vs-a-permanent-table)
Om du är intresserad av hur filsystem fungerar, har [Understanding the Linux Kernel](https://www.oreilly.com/library/view/understanding-the-linux/0596005652/) ett bra kapitel om det.