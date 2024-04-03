---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:20.020414-07:00
description: "Jak to zrobi\u0107: W Fish Shell mo\u017Cesz pisa\u0107 do stderr, przekierowuj\u0105\
  c swoje wyj\u015Bcie za pomoc\u0105 `>&2`. Oto podstawowy przyk\u0142ad."
lastmod: '2024-03-13T22:44:35.858206-06:00'
model: gpt-4-0125-preview
summary: "W Fish Shell mo\u017Cesz pisa\u0107 do stderr, przekierowuj\u0105c swoje\
  \ wyj\u015Bcie za pomoc\u0105 `>&2`."
title: "Pisanie do standardowego b\u0142\u0119du"
weight: 25
---

## Jak to zrobić:
W Fish Shell możesz pisać do stderr, przekierowując swoje wyjście za pomocą `>&2`. Oto podstawowy przykład:

```fish
echo "To jest komunikat o błędzie" >&2
```

To polecenie po prostu wyświetla wiadomość do stderr zamiast do stdout. Jeśli chciałbyś napisać skrypt, który wypisuje zarówno regularne, jak i komunikaty o błędach, możesz zrobić coś takiego:

```fish
echo "Rozpoczynanie procesu"
echo "Wystąpił błąd" >&2
echo "Proces zakończony"
```

Przykładowe wyjście, jeśli uruchomisz skrypt i przekierujesz stderr do pliku:

```
Rozpoczynanie procesu
Proces zakończony
```

Komunikat o błędzie nie pojawi się na standardowym wyjściu, ale zostanie znaleziony w pliku, do którego przekierowałeś stderr.

W scenariuszach wymagających bardziej zaawansowanego obsługiwania błędów lub logowania, Fish nie zawiera wbudowanych bibliotek specjalnie przeznaczonych do tego celu. Jednak możesz wykorzystać zewnętrzne narzędzia lub napisać funkcje pomocnicze. Na przykład tworzenie prostej funkcji logującej może wyglądać tak:

```fish
function log_error
    echo $argv >&2
end

log_error "To jest zaawansowany komunikat o błędzie"
```

Ta funkcja `log_error` przyjmie dowolny ciąg, który jej podasz, i zapisze go do stderr. Używanie funkcji tego typu może pomóc zachować czytelność i spójność obsługi błędów w całych twoich skryptach.
