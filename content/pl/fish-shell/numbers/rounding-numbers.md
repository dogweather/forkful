---
date: 2024-01-26 03:44:22.228952-07:00
description: "Jak to zrobi\u0107: W Fish, zaokr\u0105glanie liczb opiera si\u0119\
  \ na komendzie `math`. U\u017Cyj `math -s0`, aby zaokr\u0105gli\u0107 do najbli\u017C\
  szej liczby ca\u0142kowitej."
lastmod: '2024-03-13T22:44:35.833838-06:00'
model: gpt-4-0125-preview
summary: "W Fish, zaokr\u0105glanie liczb opiera si\u0119 na komendzie `math`."
title: "Zaokr\u0105glanie liczb"
weight: 13
---

## Jak to zrobić:
W Fish, zaokrąglanie liczb opiera się na komendzie `math`. Użyj `math -s0`, aby zaokrąglić do najbliższej liczby całkowitej.

```fish
# Zaokrąglanie w górę
echo (math -s0 "4.7")
# Wyjście: 5

# Zaokrąglanie w dół
echo (math -s0 "4.3")
# Wyjście: 4

# Zaokrąglanie do dwóch miejsc dziesiętnych
echo (math -s2 "4.5678")
# Wyjście: 4.57

# Zaokrąglanie liczby ujemnej
echo (math -s0 "-2.5")
# Wyjście: -3
```

## Dogłębna analiza
Historycznie, zaokrąglanie liczb było wykonywane bardziej ręcznie lub z użyciem zewnętrznych narzędzi, ale we współczesnych powłokach takich jak Fish jest to wbudowana funkcjonalność. Podejście Fish przy użyciu komendy `math` upraszcza sprawy w porównaniu ze starszymi powłokami. Alternatywy w innych środowiskach programistycznych się różnią; języki takie jak Python używają funkcji takich jak `round()`, podczas gdy Bash może wymagać bardziej skomplikowanych wyrażeń lub narzędzia `bc`. Implementacja zaokrąglania w Fish upraszcza skryptowanie, utrzymując matematykę wewnątrz środowiska powłoki zamiast wywoływać inne narzędzia lub języki.

## Zobacz również
- Dokumentacja Fish dla komendy `math`: https://fishshell.com/docs/current/cmds/math.html
- Standard IEEE dla arytmetyki zmiennoprzecinkowej (IEEE 754): https://ieeexplore.ieee.org/document/4610935
