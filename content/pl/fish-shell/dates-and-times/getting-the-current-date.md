---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:38.513860-07:00
description: "Jak to zrobi\u0107: Fish Shell wykorzystuje zewn\u0119trzne polecenia\
  \ takie jak `date` do pobierania bie\u017C\u0105cej daty, oferuj\u0105c elastyczno\u015B\
  \u0107 w formatowaniu wyj\u015Bcia\u2026"
lastmod: '2024-03-13T22:44:35.852231-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell wykorzystuje zewn\u0119trzne polecenia takie jak `date` do pobierania\
  \ bie\u017C\u0105cej daty, oferuj\u0105c elastyczno\u015B\u0107 w formatowaniu wyj\u015B\
  cia wed\u0142ug potrzeb."
title: Pobieranie aktualnej daty
weight: 29
---

## Jak to zrobić:
Fish Shell wykorzystuje zewnętrzne polecenia takie jak `date` do pobierania bieżącej daty, oferując elastyczność w formatowaniu wyjścia według potrzeb. Oto jak tego użyć:

```fish
# Wyświetl bieżącą datę w domyślnym formacie
echo (date)

# Przykład wyjścia: Śro 25 Paź 2023 15:42:03 BST
```

Aby dostosować format daty, można użyć opcji `+` po której następują specyfikatory formatu:

```fish
# Wyświetl bieżącą datę w formacie RRRR-MM-DD
echo (date "+%Y-%m-%d")

# Przykład wyjścia: 2023-10-25
```

Do bardziej złożonych zadań, takich jak praca ze znacznikami czasu lub wykonywanie arytmetyki dat, Fish Shell opiera się na zewnętrznych narzędziach takich jak `date` ze względu na jego skryptowy charakter. Oto przykład pobierania bieżącego znacznika czasu UNIX:

```fish
# Pobierz bieżący znacznik czasu UNIX
echo (date "+%s")

# Przykład wyjścia: 1666710123
```

Aby dodać jeden dzień do bieżącej daty za pomocą `date`:

```fish
# Dodaj jeden dzień do bieżącej daty
echo (date -d "+1 day" "+%Y-%m-%d")

# Przykład wyjścia: 2023-10-26
```

Uwaga: Przykłady korzystają z opcji polecenia `date`, które działają z GNU coreutils. Opcje mogą się różnić w innych środowiskach, takich jak macOS, które domyślnie używa polecenia BSD date. Zawsze należy odwoływać się do `date --help` lub strony man, aby uzyskać szczegóły specyficzne dla Twojego środowiska.
