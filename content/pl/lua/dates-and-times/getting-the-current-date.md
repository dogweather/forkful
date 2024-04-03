---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:25.613161-07:00
description: "Pobieranie aktualnej daty w programowaniu jest kluczowym zadaniem dla\
  \ wielu aplikacji, w tym zapisywania log\xF3w, znakowania czasu zdarze\u0144 czy\
  \ planowania\u2026"
lastmod: '2024-03-13T22:44:35.550550-06:00'
model: gpt-4-0125-preview
summary: "Pobieranie aktualnej daty w programowaniu jest kluczowym zadaniem dla wielu\
  \ aplikacji, w tym zapisywania log\xF3w, znakowania czasu zdarze\u0144 czy planowania\
  \ zada\u0144."
title: Pobieranie aktualnej daty
weight: 29
---

## Co i dlaczego?

Pobieranie aktualnej daty w programowaniu jest kluczowym zadaniem dla wielu aplikacji, w tym zapisywania logów, znakowania czasu zdarzeń czy planowania zadań. W języku Lua, ta funkcjonalność pozwala programistom na bezproblemowe zarządzanie operacjami związanymi z datą i czasem w swoich aplikacjach, zapewniając skuteczną interakcję ich oprogramowania z danymi w czasie rzeczywistym.

## Jak to zrobić:

Lua udostępnia funkcję `os.date` do pobierania aktualnej daty i czasu. Funkcję można użyć bez argumentów, aby otrzymać sformatowany ciąg znaków, lub z określonymi specyfikatorami formatu, aby dostosować wyjście. Oto jak jej używać:

```lua
-- Pobieranie aktualnej daty i czasu jako sformatowany ciąg znaków
print(os.date())  -- np., Czw Mar  3 14:02:03 2022

-- Dostosowywanie formatu wyjściowego
-- %Y dla roku, %m dla miesiąca, %d dla dnia, %H dla godziny, %M dla minut
print(os.date("%Y-%m-%d %H:%M"))  -- np., 2022-03-03 14:02
```

Dla bardziej zaawansowanej manipulacji datą i czasem, Lua nie ma wbudowanych bibliotek tak rozbudowanych jak niektóre inne języki programowania. Jednak można użyć bibliotek stron trzecich, takich jak `lua-date` (https://github.com/Tieske/date). Ta biblioteka oferuje bardziej wszechstronne funkcjonalności dla manipulacji datami i czasem. Oto jak możesz jej użyć:

Najpierw, upewnij się, że zainstalowałeś bibliotekę `lua-date`. Zazwyczaj możesz ją zainstalować za pomocą LuaRocks, używając poniższego polecenia:

```bash
luarocks install lua-date
```

Następnie możesz użyć jej w swoim skrypcie Lua w następujący sposób:

```lua
local date = require("date")

-- Tworzenie obiektu daty dla aktualnej daty i czasu
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- np., 2022-03-03 14:02:03
```

Ten przykład demonstruje tworzenie obiektu `date` reprezentującego obecny moment, który następnie możesz formatować w sposób podobny do funkcji `os.date`, ale z dodatkową elastycznością i opcjami oferowanymi przez bibliotekę `lua-date`.
