---
title:                "Konwersja ciągu znaków na małe litery"
aliases:
- pl/fish-shell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:28.705676-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja ciągu znaków na małe litery"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Konwersja ciągu znaków na małe litery polega na zmianie wszystkich wielkich liter w napisie na ich małe odpowiedniki. Programiści robią to, by ujednolicić dane, np. przy porównywaniu ciągów bez uwzględniania wielkości liter.

## Jak to zrobić:
W Fish, przekształcanie tekstu na małe litery jest takie proste, jak użycie wbudowanej funkcji `string`. Oto jak to działa:

```Fish Shell
echo "WITAJ ŚWIECIE!" | string lower
```

Wynik:
```
witaj świecie!
```

## Zanurkujmy głębiej
Dawniej mogłeś potrzebować narzędzi zewnętrznych jak `tr` albo `awk` do manipulacji tekstem. Na przykład w Bashu mógłbyś napisać `tr '[:upper:]' '[:lower:]'`, by zmienić wielkie litery na małe. W Fish, funkcja `string` pojawiła się w wersji 2.3.0, dodając proste narzędzia do manipulacji ciągami. Alternatywnie, możesz używać starszych poleceń Unix, ale `string` jest szybsze i bardziej wygodne w używaniu.

Dlaczego używać wbudowanej funkcji Fish `string`? To pisane specjalnie pod Fish Shell narzędzie jest nie tylko szybsze, ale też bardziej czytelne i mniej podatne na błędy. Dzięki temu twój kod jest bardziej przystępny i łatwiejszy w utrzymaniu.

Jeśli chodzi o implementację, `string` jest funkcją wbudowaną, więc wykonuje się szybko i efektywnie. Działa bezproblemowo z innymi komendami Fish Shell, jak pętle czy operatory warunkowe, pozwalając na łatwą integrację z kodem.

## Zobacz również:
- Oficjalna dokumentacja `string`: https://fishshell.com/docs/current/cmds/string.html
- Poradnik do Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Ogólna dokumentacja fish shell: https://fishshell.com/docs/current/index.html

Pamiętaj, by sprawdzić dokumentację Fish dla najnowszych informacji i praktyk.
