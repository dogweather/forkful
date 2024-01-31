---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:33:01.254344-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"

category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsing HTML to proces ekstrahowania danych z dokumentów HTML. Programiści to robią, aby uzyskać dostęp do treści i struktury stron internetowych, które zwykle są wyświetlane w przeglądarkach.

## Jak to zrobić:
Świetnym narzędziem do parsowania HTML w Lua jest biblioteka `lua-htmlparser`. Zaczynamy od jej instalacji:

```Lua
luarocks install lua-htmlparser
```

Następnie, możesz użyć poniższego kodu, aby wyodrębnić elementy z przykładowego HTML:

```Lua
local HtmlParser = require "htmlparser"

local html = [[
<!DOCTYPE html>
<html>
<head>
    <title>Nasz przykładowy tytuł</title>
</head>
<body>
    <h1>Witaj świecie!</h1>
    <p>To jest przykładowy akapit.</p>
</body>
</html>
]]

local parser = HtmlParser.parse(html)
local body = parser:select('body')[1]
print(body:getcontent())
```

Wyjście będzie wyglądać tak:
```
    <h1>Witaj świecie!</h1>
    <p>To jest przykładowy akapit.</p>
```

## Deep Dive
Parsowanie HTML jest starym problemem - od kiedy tylko strony internetowe stały się popularne. W Lua, używamy bibliotek jak `lua-htmlparser`, dlatego że sam język nie ma wbudowanego wsparcia dla tej funkcjonalności. Alternatywy to użycie wyrażeń regularnych (ale to złe praktyki) lub narzędzia w stylu `luasoup`, które przenosi wygodę BeautifulSoup z Pythona. Implementacja parsowania polega na przechodzeniu przez drzewo DOM dokumentu HTML i wyodrębnianiu interesujących nas informacji.

## Zobacz też
- [lua-htmlparser na GitHubie](https://github.com/msva/lua-htmlparser)
- [Dokumentacja Lua](https://www.lua.org/docs.html)
- [Beautiful Soup, inspiracja dla luasoup](https://www.crummy.com/software/BeautifulSoup/)
