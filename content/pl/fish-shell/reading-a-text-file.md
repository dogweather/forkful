---
title:                "Fish Shell: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś początkującym programistą, zapewne słyszałeś o możliwości czytania plików tekstowych za pomocą Fish Shell. Ale może zastanawiasz się, dlaczego jest to ważne i dlaczego warto się dowiedzieć jak to zrobić. W tym blogu postaram się wytłumaczyć, czemu warto poznać tę umiejętność.

## Jak to zrobić

Do czytania plików tekstowych w Fish Shell możemy użyć komendy ```cat```. Poniżej przedstawiam przykładowy kod oraz oczekiwaną odpowiedź:

```Fish Shell
cat hello.txt
```

```
Hello, World!
```

W tym przykładzie, ```cat``` jest komendą, a ```hello.txt``` jest plikiem tekstowym, który chcemy przeczytać. To proste i przydatne narzędzie, które warto mieć w swoim programistycznym arsenale.

## Od podstaw

Teraz, że już wiesz jak użyć komendy ```cat```, pora na głębsze zanurzenie się w czytanie plików tekstowych w Fish Shell. Istnieją różne opcje, jakie możesz dodać do tej komendy, aby uzyskać różne wyniki. Na przykład, możesz użyć flagi ```-n```, aby wyświetlić numerowane wiersze lub flagi ```-r``` do czytania pliku od końca. Możesz też użyć ```|``` do przesyłania wyjścia do innego polecenia, lub ```>>``` do przekierowania wyjścia do nowego pliku.

## Zobacz też

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/cmds/cat.html)
- [Przewodnik po komendzie cat](https://www.howtogeek.com/435903/how-to-use-the-cat-command-in-linux)
- [Inne przydatne komendy Fish Shell](https://fishshell.com/docs/current/commands.html)