---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:00.408037-07:00
description: "Pobieranie aktualnej daty w Bashu polega na u\u017Cywaniu wbudowanych\
  \ polece\u0144 do wy\u015Bwietlania daty i czasu w r\xF3\u017Cnych formatach. Programi\u015B\
  ci u\u017Cywaj\u0105 tej\u2026"
lastmod: '2024-03-11T00:14:08.787076-06:00'
model: gpt-4-0125-preview
summary: "Pobieranie aktualnej daty w Bashu polega na u\u017Cywaniu wbudowanych polece\u0144\
  \ do wy\u015Bwietlania daty i czasu w r\xF3\u017Cnych formatach. Programi\u015B\
  ci u\u017Cywaj\u0105 tej\u2026"
title: Pobieranie aktualnej daty
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie aktualnej daty w Bashu polega na używaniu wbudowanych poleceń do wyświetlania daty i czasu w różnych formatach. Programiści używają tej funkcjonalności do zadań takich jak dodawanie znaczników czasowych do logów, planowanie zadań lub po prostu jako część swoich skryptów informacji systemowych, aby śledzić, kiedy wykonano akcje.

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
