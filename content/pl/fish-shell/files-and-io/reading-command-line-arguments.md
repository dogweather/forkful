---
date: 2024-01-20 17:55:47.932341-07:00
description: "How to: (Jak to zrobi\u0107?) Podawanie argument\xF3w i odczytywanie\
  \ ich w Fish jest banalnie proste. Oto jak."
lastmod: '2024-04-05T21:53:37.283754-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107?) Podawanie argument\xF3w i odczytywanie ich w Fish\
  \ jest banalnie proste."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

## How to: (Jak to zrobić?)
Podawanie argumentów i odczytywanie ich w Fish jest banalnie proste. Oto jak:

```Fish Shell
# wywołanie skryptu z argumentami
fish my_script.fish arg1 arg2

# my_script.fish
for arg in $argv
    echo "Argument: $arg"
end
```

Output:
```
Argument: arg1
Argument: arg2
```

Możesz też łatwo dostać się do konkretnych argumentów:

```Fish Shell
# Pierwszy argument
echo $argv[1]

# Drugi argument
echo $argv[2]
```

## Deep Dive (Głębsze spojrzenie)
Fish Shell ma prostą i przejrzystą składnię, a obsługa argumentów linii komend nie jest wyjątkiem. Kiedy UNIX powstał, argumenty wywołania pozwoliły użytkownikom na interaktywną manipulację działaniem programów. W Bashu, innym popularnym shellowi, używa się `$1`, `$2` dla kolejnych argumentów – w Fish, lista `$argv` ułatwia pracę z wieloma argumentami. Alternatywą dla argumentów są pliki konfiguracyjne lub interaktywne wprowadzanie danych, ale to może być mniej wydajne przy automatyzacji zadań.

## See Also (Zobacz także)
- [Dokumentacja Fish Shell o zmiennej argv](https://fishshell.com/docs/current/language.html#variables)
- [Poradnik komend Fish](https://fishshell.com/docs/current/tutorial.html#tut_scripting)
- [Unix Programming Environment](https://en.wikipedia.org/wiki/The_Unix_Programming_Environment) – książka wprowadzająca w kontekst historyczny
- [Bash Scripting Tutorial](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/) – w przypadku potrzeby porównania z Bashem
