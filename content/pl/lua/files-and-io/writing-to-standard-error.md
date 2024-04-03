---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:48.261756-07:00
description: "Pisanie do standardowego b\u0142\u0119du (stderr) polega na kierowaniu\
  \ komunikat\xF3w o b\u0142\u0119dach oraz wyj\u015B\u0107 diagnostycznych do osobnego\
  \ kana\u0142u, r\xF3\u017Cnego od standardowego\u2026"
lastmod: '2024-03-13T22:44:35.556455-06:00'
model: gpt-4-0125-preview
summary: "Pisanie do standardowego b\u0142\u0119du (stderr) polega na kierowaniu komunikat\xF3\
  w o b\u0142\u0119dach oraz wyj\u015B\u0107 diagnostycznych do osobnego kana\u0142\
  u, r\xF3\u017Cnego od standardowego wyj\u015Bcia (stdout)."
title: "Pisanie do standardowego b\u0142\u0119du"
weight: 25
---

## Jak to zrobić:
W Lua, pisanie do stderr można osiągnąć za pomocą funkcji `io.stderr:write()`. Oto jak można napisać prosty komunikat o błędzie do standardowego błędu:

```lua
io.stderr:write("Błąd: Nieprawidłowe wejście.\n")
```

Jeśli potrzebujesz wyświetlić zmienną lub połączyć kilka fragmentów danych, skonkatenuj je w funkcji write:

```lua
local errorMessage = "Nieprawidłowe wejście."
io.stderr:write("Błąd: " .. errorMessage .. "\n")
```

**Przykładowe wyjście na stderr:**
```
Błąd: Nieprawidłowe wejście.
```

W bardziej skomplikowanych scenariuszach, lub przy pracy z większymi aplikacjami, warto rozważyć biblioteki logowania stron trzecich, takie jak LuaLogging. Z LuaLogging możesz kierować logi do różnych miejsc docelowych, w tym stderr. Oto krótki przykład:

Najpierw upewnij się, że LuaLogging jest zainstalowane za pomocą LuaRocks:

```
luarocks install lualogging
```

Następnie, aby napisać komunikat o błędzie do stderr przy użyciu LuaLogging:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Błąd: Nieprawidłowe wejście.")
```

To podejście oferuje zaletę standaryzowanego logowania w całej aplikacji, z dodatkową elastycznością ustawiania poziomów logowania (np. ERROR, WARN, INFO) za pomocą prostego API.
