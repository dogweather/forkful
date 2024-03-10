---
date: 2024-01-19
description: "Wyra\u017Cenia regularne to wzorce u\u017Cywane do wyszukiwania i manipulowania\
  \ tekstami. Programi\u015Bci u\u017Cywaj\u0105 ich, bo pozwalaj\u0105 na zaawansowane\
  \ przetwarzanie tekstu\u2026"
lastmod: '2024-03-09T21:11:18.404710-07:00'
model: unknown
summary: "Wyra\u017Cenia regularne to wzorce u\u017Cywane do wyszukiwania i manipulowania\
  \ tekstami. Programi\u015Bci u\u017Cywaj\u0105 ich, bo pozwalaj\u0105 na zaawansowane\
  \ przetwarzanie tekstu\u2026"
title: "Wykorzystanie wyra\u017Ce\u0144 regularnych"
---

{{< edit_this_page >}}

## What & Why? - Co i dlaczego?
Wyrażenia regularne to wzorce używane do wyszukiwania i manipulowania tekstami. Programiści używają ich, bo pozwalają na zaawansowane przetwarzanie tekstu niewielkim kosztem.

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
