---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:01:35.003642-07:00
description: "Jak to zrobi\u0107: W Fish piszesz funkcj\u0119 s\u0142owem kluczowym\
  \ `function`, nadajesz jej nazw\u0119 i ko\u0144czysz `end`. Oto prosta funkcja."
lastmod: '2024-03-13T22:44:35.847047-06:00'
model: gpt-4-0125-preview
summary: "W Fish piszesz funkcj\u0119 s\u0142owem kluczowym `function`, nadajesz jej\
  \ nazw\u0119 i ko\u0144czysz `end`."
title: Organizacja kodu w funkcje
weight: 18
---

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
