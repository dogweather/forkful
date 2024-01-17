---
title:                "Wczytywanie argumentów wiersza poleceń"
html_title:           "Gleam: Wczytywanie argumentów wiersza poleceń"
simple_title:         "Wczytywanie argumentów wiersza poleceń"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O co chodzi?

Czy kiedykolwiek zastanawiałeś się, jak programy w terminalu są w stanie odtworzyć polecenia, które wpisujesz po zakończeniu ich uruchamiania? Właśnie wtedy wychodzi na scenę czytanie argumentów wiersza poleceń. Programiści często wykorzystują tę umiejętność, aby wpływać na zachowanie swoich programów poprzez różne opcje lub parametry.

## Jak to zrobić:

Możesz łatwo odczytać argumenty wiersza poleceń w Gleam dzięki modułowi ```sys```. W poniższym przykładzie użyjemy go do wyświetlenia wszystkich przekazanych argumentów:

```Gleam
let args = sys.args
List.foreach(args, fn(arg) {
  IO.println(arg)
})
```

Kiedy uruchomisz ten kod z argumentami ```"Gleam"``` i ```"jest"```, zobaczysz następujący wynik:

```Gleam
"bin/program"
"Gleam"
"jest"
```

## Głębszy zanurzenie:

Korzystanie z argumentów wiersza poleceń stało się powszechne w programowaniu terminalowym z powodu ich prostoty i wszechstronności. Alternatywnymi sposobami przekazywania informacji do programów są zapisywanie ich w plikach konfiguracyjnych lub ustawieniach programu. Jednak czytanie argumentów wiersza poleceń jest szybsze i nie wymaga edycji dodatkowych plików.

Moduł ```sys``` w Gleam dostarcza również funkcje umożliwiające dostęp do wielu innych komponentów systemu operacyjnego, w tym zmiennych środowiskowych i ścieżki do bieżącego katalogu. Dlatego używanie go jest bardzo przydatne podczas tworzenia programów, które muszą interakcjonować z systemem.

## Zobacz również:

Jeśli jesteś zainteresowany dowiedzeniem się więcej na temat modułu ```sys``` w Gleam, możesz przeczytać dokumentację na oficjalnej stronie języka: https://gleam.run/modules/sys.html.

Jeśli chciałbyś poznać inne sposoby przekazywania danych do programów w terminalu, możesz zapoznać się z różnymi opcjami w narzędziu ```argparse```: https://github.com/moscow-python-raise/dotstarsuperstar/tree/master/ReadTheDocs/argparse.

Pamiętaj, że czytanie argumentów wiersza poleceń jest jedną z podstawowych umiejętności każdego programisty, więc warto zrobić to porządnie!