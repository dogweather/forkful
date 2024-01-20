---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaczynamy nowy projekt po to, by zorganizować nasze pliki kodu i ułatwić współpracę. Faktem jest, że programiści zaczynają nowe projekty, aby skutecznie rozwiązywać problemy, uzyskać czysty kod i zorganizować proces twórczy.

## Jak to zrobić:
Tworzenie nowego projektu w Gleam (aktualna wersja) jest proste. Używamy wiersza polecenia i wprowadzamy odpowiednie polecenia.

```Gleam
$ rebar3 new gleam_lib my_project
$ cd my_project
```

Wynikiem jest nowy projekt Gleam o nazwie "my_project". Zawiera również podstawową strukturę biblioteki.

## Głębsze spojrzenie
Tworzenie nowych projektów to nie nowy koncept. Stworzony wiele lat temu, ma na celu pomóc programistom w lepszym zarządzaniu ich kodem. W Gleam, mamy alternatywe dla `rebar3 new`, które to jest `gleam new`. `gleam new` tworzy nowy projekt Gleam o podanej nazwie, wykorzystując domyślny szablon.

```Gleam
$ gleam new my_project
$ cd my_project
```

W przypadku implementacji, nowy projekt składa się z katalogów src, test i gen, a także plików rebar.config i .gitignore. W rzeczywistości, struktura projektu oraz pliki mogą się różnić w zależności od konkretnych potrzeb projektu.

## Zobacz także
Aby uzyskać więcej informacji na temat Gleam, odwiedź:
- Oficjalna strona Gleam: https://gleam.run/
- Dokumentacja Gleam: https://gleam.run/book/
- Źródło Gleam na GitHubie: https://github.com/gleam-lang/gleam
- Lista narzędzi do Gleam: https://gleam.run/tools/