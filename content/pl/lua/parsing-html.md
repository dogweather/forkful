---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Analiza HTML (parsing HTML) polega na przetworzeniu kodu HTML na strukturę danych, która jest zrozumiała dla komputera. Programiści robią to, aby móc manipulować strukturą strony internetowej lub analizować jej zawartość.

## Jak to zrobić:
```Lua
local lom = require 'lxp.lom'

local contents = [[<html>
<head>
	<title>Moja Strona WWW</title>
</head>
<body>
	<h1>Witaj Świecie!</h1>
</body>
</html>]]

local doc = lom.parse(contents)

for i, element in ipairs(doc) do
	if type(element) == "table" then
		print(element.tag)
	end
end
```
Wyjście:
```
head
body
```

## W Głąb Tematu
(1) W kontekście historycznym, parsing HTML stał się koniecznością z powodu coraz większej złożoności stron internetowych. Programiści potrzebują narzędzi, które pomogą im zrozumieć i manipulować tą złożonością.
(2) Alternatywą do biblioteki Lua 'lxp.lom' do analizy HTML jest 'htmlparser'. Wybór zależy od konkretnej potrzeby, są one jednak podobne w użyciu.
(3) Przy implementacji, biblioteka 'lxp.lom' przetwarza kod HTML na strukturę drzewiastą (tree structure), co pozwala na łatwe poruszanie się po jego elementach i manipulowanie nimi.

## Zobacz także:
- Dokumentacja Lua 'lxp.lom': http://matthewwild.co.uk/projects/luaexpat/lom.html
- Dokumentacja Lua 'htmlparser': https://github.com/msva/lua-htmlparser
- Podstawy analizy składniowej (Parsing): https://pl.wikipedia.org/wiki/Analiza_sk%C5%82adniowa