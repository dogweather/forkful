---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:00.408037-07:00
description: "Jak to zrobi\u0107: W Bashu polecenie `date` jest twoim g\u0142\xF3\
  wnym narz\u0119dziem do pobierania aktualnej daty i czasu. Oto kilka przyk\u0142\
  ad\xF3w, jak mo\u017Cna go u\u017Cywa\u0107: 1.\u2026"
lastmod: '2024-03-13T22:44:35.595713-06:00'
model: gpt-4-0125-preview
summary: "W Bashu polecenie `date` jest twoim g\u0142\xF3wnym narz\u0119dziem do pobierania\
  \ aktualnej daty i czasu."
title: Pobieranie aktualnej daty
weight: 29
---

## Jak to zrobić:
W Bashu polecenie `date` jest twoim głównym narzędziem do pobierania aktualnej daty i czasu. Oto kilka przykładów, jak można go używać:

1. **Pobierz aktualną datę i czas w domyślnym formacie:**

```bash
date
```

*Przykładowe wyjście:*
```
Śro, 5 kwi 14:22:04 PDT 2023
```

2. **Dostosuj format wyjściowy:** Możesz określić format wyjściowy, używając specyfikatorów formatu `+%`. Na przykład, aby wyświetlić datę w formacie RRRR-MM-DD:

```bash
date "+%Y-%m-%d"
```

*Przykładowe wyjście:*
```
2023-04-05
```

3. **Pobierz aktualną sygnaturę czasową UNIX:** Sygnatura czasowa UNIX to liczba sekund od Epoki UNIX (1 stycznia 1970). Jest to przydatne dla skryptów, które wykonują obliczenia oparte na różnicach czasowych.

```bash
date "+%s"
```

*Przykładowe wyjście:*
```
1672877344
```

Do tej podstawowej operacji w Bashu zazwyczaj nie używa się popularnych bibliotek firm trzecich, ponieważ wbudowane polecenie `date` zapewnia obszerną funkcjonalność. Jednakże, do bardziej zaawansowanych manipulacji datą i czasem, programiści mogą używać innych języków programowania lub narzędzi oferujących biblioteki do arytmetyki i analizy dat, takie jak moduł `datetime` w Pythonie.
