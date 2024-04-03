---
date: 2024-01-20 17:38:28.705676-07:00
description: "Jak to zrobi\u0107: W Fish, przekszta\u0142canie tekstu na ma\u0142\
  e litery jest takie proste, jak u\u017Cycie wbudowanej funkcji `string`. Oto jak\
  \ to dzia\u0142a."
lastmod: '2024-03-13T22:44:35.825581-06:00'
model: gpt-4-1106-preview
summary: "W Fish, przekszta\u0142canie tekstu na ma\u0142e litery jest takie proste,\
  \ jak u\u017Cycie wbudowanej funkcji `string`."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

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
