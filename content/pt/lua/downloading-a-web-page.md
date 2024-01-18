---
title:                "Baixando uma página da web"
html_title:           "Lua: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que e por que?

Baixar uma página da web geralmente se refere ao processo de obter o conteúdo de uma página da web e salvá-lo em seu computador. Isso é feito por programadores por várias razões, como extrair dados específicos de uma página da web, automação de tarefas, testes de software e muito mais.

## Como fazer:

### Exemplo 1: Baixando o conteúdo de uma página da web

```
local http = require("socket.http")
local page = http.request("http://minha_pagina.com")
print(page)
```

```
<html>
<head>
<title>Minha Página</title>
</head>
<body>
<h1>Bem-vindo à minha página!</h1>
<p>Aqui você encontrará informações úteis.</p>
</body>
</html>
```

### Exemplo 2: Salvando o conteúdo de uma página da web em um arquivo

```
local http = require("socket.http")
local page = http.request("http://minha_pagina.com")
local file = io.open("minha_pagina.html", "w")
file:write(page)
file:close()
```

## Mergulho profundo:

Baixar páginas da web tem sido um recurso importante para programadores desde o início da internet. Existem várias ferramentas e linguagens de programação que podem ser usadas para baixar conteúdo da web, como Python, Java, PHP e, claro, Lua. Dependendo do objetivo, diferentes técnicas e algoritmos podem ser usados para otimizar o processo de download. As APIs REST (representational state transfer) também são amplamente utilizadas para baixar dados de uma página da web.

## Veja também:

- [documentação oficial LuaSocket](https://github.com/diegonehab/luasocket)
- [artigo tutorial sobre baixar páginas da web com Lua](https://www.lua.org/wshop12/Lucas%20Baier%20Bernardescu.pdf)
- [exemplo de uso das APIs REST em Lua](https://www.lua.org/pil/22.1.html)