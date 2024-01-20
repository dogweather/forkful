---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pobieranie strony internetowej wiąże się z uzyskaniem jej kopii na swoim komputerze. Programiści robią to, na przykład, aby analizować dane z tej strony lub tworzyć kopie zapasowe strony dla późniejszego użytku.

## Jak to zrobić:

Użyjemy biblioteki `lua-requests` do pobrania zawartości strony internetowej. Poniżej jest przykładowy kod:

```Lua
local requests = require("requests")

local response = requests.get('http://example.com')

print(response.status_code)
print(response.text)
```

Kiedy uruchomisz powyższy kod, zobaczysz coś podobnego do poniższego:
```Lua
200
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## W Głąb Tematu 

Pierwsze narzędzia do pobierania stron internetowych powstały już na początku lat 90. XX wieku. Z czasem technologia przeszła wiele ewolucji, a dzisiaj mamy wiele alternatyw, takich jak curl, wget, httpie czy biblioteki języków programowania do wykonywania tego zadania.

W Lua, `lua-requests` jest popularną biblioteką do robienia http i https żądań z wykorzystaniem składni podobnej do tej używanej w Pythonie. Wykorzystuje ona pod maską moduł `luasocket` do niskopoziomowych operacji sieciowych.

## Zobacz też 

- [Dokumentacja lua-requests](https://github.com/JakobGreen/lua-requests)
- [Dokumentacja luasocket](http://w3.impa.br/~diego/software/luasocket/)
- [Tutorial luasocket](http://lua-users.org/wiki/LuaSocketTutorial)