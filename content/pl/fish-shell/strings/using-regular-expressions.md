---
date: 2024-01-19
description: "How to: - Jak to zrobi\u0107: Znajdowanie s\u0142\xF3w zaczynaj\u0105\
  cych si\u0119 na \"pl\" w pliku `tekst.txt`."
lastmod: '2024-04-05T22:37:44.587256-06:00'
model: unknown
summary: "- Jak to zrobi\u0107: Znajdowanie s\u0142\xF3w zaczynaj\u0105cych si\u0119\
  \ na \"pl\" w pliku `tekst.txt`."
title: "Wykorzystanie wyra\u017Ce\u0144 regularnych"
weight: 11
---

## How to: - Jak to zrobić:
Znajdowanie słów zaczynających się na "pl" w pliku `tekst.txt`:

```Fish Shell
grep '^pl' tekst.txt
```

Zastępowanie "fish" słowem "shell" w `skrypt.fish`:

```Fish Shell
sed 's/fish/shell/g' skrypt.fish > skrypt_zmieniony.fish
```

Wyszukiwanie wszystkich plików `.fish` wyświetlając linie pasujące do wzorca "function":

```Fish Shell
grep 'function' *.fish
```

## Deep Dive - Zagłębienie się
Wyrażenia regularne powstały w latach 50, kiedy to Stephen Cole Kleene opisał teoretyczny model zwany "regular expressions". Obecnie istnieje wiele odmian, np. POSIX czy Perl-Compatible Regular Expressions (PCRE), a każda implementacja ma swoje unikalne cechy. W skorupce Fish zazwyczaj korzysta się z narzędzi jak `grep`, `sed`, `awk`, które używają regular expressions w sposób charakterystyczny dla systemu Unix.

## See Also - Zobacz też
Dokumentacja `grep`: https://www.gnu.org/software/grep/manual/grep.html

Dokumentacja `sed`: https://www.gnu.org/software/sed/manual/sed.html

Tutorial wyrażeń regularnych: https://www.regular-expressions.info/tutorial.html

Podręcznik skorupki Fish: https://fishshell.com/docs/current/index.html
