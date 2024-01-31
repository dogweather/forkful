---
title:                "Organizacja kodu w funkcje"
date:                  2024-01-28T23:01:35.003642-07:00
model:                 gpt-4-0125-preview
simple_title:         "Organizacja kodu w funkcje"

category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, dogweather, reviewed and added links
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Organizowanie kodu w funkcje polega na grupowaniu fragmentów skryptu do realizacji konkretnych zadań. Robimy to, ponieważ sprawia to, że kod jest łatwiejszy do przeczytania, przetestowania i ponownego wykorzystania - nikt nie chce przebijać się przez bagno kodu spaghetti.

## Jak to zrobić:
W Fish piszesz funkcję słowem kluczowym `function`, nadajesz jej nazwę i kończysz `end`. Oto prosta funkcja:

```fish
function hello
    echo "Hello, World!"
end

hello
```

Wynik:
```
Hello, World!
```

Teraz, zróbmy tak, aby powitała użytkownika:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

Wynik:
```
Hey there, your_username!
```

Aby zachować ją na przyszłe sesje, użyj `funcsave greet`.

## Wnikliwe spojrzenie
Funkcje w Fish Shell są jak mini-skrypty — możesz tam włożyć prawie wszystko. Historycznie, koncepcja funkcji w skryptach shell zaoszczędziła niezliczone godziny powtarzalnego pisania i debugowania. W przeciwieństwie do języków programowania takich jak Python, funkcje Shell są bardziej o wygodzie niż o strukturze.

Niektóre shelle, takie jak Bash, używają `function` lub po prostu nawiasów klamrowych. Fish trzyma się `function ... end` — jasne i czytelne. Wewnątrz funkcji Fish możesz korzystać ze wszystkiego, co najlepsze: parametry, lokalne zmienne z `set -l`, a nawet zdefiniować funkcję wewnątrz innej funkcji.

Nie będziesz potrzebować wartości `return`, ponieważ Fish nie kładzie na to dużego nacisku; wyjście twojej funkcji jest jej wartością zwracaną. I jeśli chcesz mieć trwałe funkcje dostępne na przyszłe sesje, pamiętaj o `funcsave`.

## Zobacz również

- Tutorial Fish na temat funkcji: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Polecenia funkcji

- [function](https://fishshell.com/docs/current/cmds/function.html) — Utwórz funkcję
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Wydrukuj lub usuń funkcje
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Zapisz definicję funkcji do katalogu autoload użytkownika
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Interaktywnie edytuj funkcję
