---
date: 2024-01-26 03:37:51.337569-07:00
description: "Usuni\u0119cie cudzys\u0142ow\xF3w z ci\u0105gu polega na usuni\u0119\
  ciu znak\xF3w cudzys\u0142owu, kt\xF3re otaczaj\u0105 ci\u0105g znak\xF3w. Programi\u015B\
  ci cz\u0119sto chc\u0105 to robi\u0107, aby oczy\u015Bci\u0107 dane\u2026"
lastmod: '2024-03-11T00:14:08.760114-06:00'
model: gpt-4-0125-preview
summary: "Usuni\u0119cie cudzys\u0142ow\xF3w z ci\u0105gu polega na usuni\u0119ciu\
  \ znak\xF3w cudzys\u0142owu, kt\xF3re otaczaj\u0105 ci\u0105g znak\xF3w. Programi\u015B\
  ci cz\u0119sto chc\u0105 to robi\u0107, aby oczy\u015Bci\u0107 dane\u2026"
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usunięcie cudzysłowów z ciągu polega na usunięciu znaków cudzysłowu, które otaczają ciąg znaków. Programiści często chcą to robić, aby oczyścić dane wejściowe, przygotować dane do celów porównawczych lub przestrzegać określonego formatu danych podczas interfejsowania z innymi programami lub systemami.

## Jak to zrobić:
Bash oferuje kilka sposobów na usunięcie cudzysłowów z ciągów znaków. Oto kilka szybkich przykładów:

```Bash
#!/bin/bash

# Korzystając z podmiany w zmiennej, aby usunąć zarówno pojedyncze, jak i podwójne cudzysłowy
STRING="\"Witaj, Świecie!\""
echo ${STRING//\"}

# Korzystając z `tr` do usunięcia cudzysłowów
STRING="'Witaj, Świecie!'"
echo $STRING | tr -d "\'"

# Korzystając z `sed` do usunięcia cudzysłowów
STRING="\"Witaj, Świecie!\""
echo $STRING | sed 's/"//g'
```

Przykładowe wyniki:

```
Witaj, Świecie!
Witaj, Świecie!
Witaj, Świecie!
```

## Szczegółowa analiza
Dawno temu, polecenia Unix jak `tr` i `sed` były głównymi narzędziami do przetwarzania tekstu. Nadal są używane dziś ze względu na ich elastyczność i moc w obsłudze transformacji tekstu, takich jak usuwanie cudzysłowów. Są one podstawowym elementem w zestawie narzędzi każdego skryptera powłoki.

Bash od tego czasu ewoluował, a substitucja zmiennych dodała kolejną warstwę prostoty do manipulacji ciągami znaków na małą skalę. Oszczędza to konieczności korzystania z zewnętrznych binariów, czyniąc skrypty nieco bardziej efektywnymi.

Choć `tr` świetnie nadaje się do usuwania znaków, nie radzi sobie z bardziej złożonymi wzorcami. Z kolei `sed` używa wyrażeń regularnych, więc czasem może być nadmiernym narzędziem i wolniejszym przy prostych operacjach.

Wybór między tymi metodami zależy od konkretnego przypadku. Jeśli musisz usunąć różne rodzaje cudzysłowów i już pracujesz w kontekście skryptu Bash, korzystanie z podmiany zmiennych jest oczywistym wyborem ze względu na jego prostotę. Ale jeśli transformujesz strumienie tekstu lub dane wieloliniowe, `tr` i `sed` są twoimi najlepszymi przyjaciółmi.

## Zobacz także:
- Podręcznik GNU Bash, szczególnie sekcje dotyczące Rozwinięcia Parametrów i Rozwinięcia Parametrów Powłoki: https://www.gnu.org/software/bash/manual/
- Podręcznik komendy `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Przegląd edytora strumienia `sed`: https://www.gnu.org/software/sed/manual/sed.html
