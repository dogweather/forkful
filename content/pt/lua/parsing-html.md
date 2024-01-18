---
title:                "Analisando html"
html_title:           "Lua: Analisando html"
simple_title:         "Analisando html"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/parsing-html.md"
---

{{< edit_this_page >}}

## O que é e por que fazer parsing HTML?

Parsing HTML é o processo de analisar o código HTML de uma página web para extrair informações específicas. Isso é feito por muitas razões, como coletar dados para análise, criar planilhas ou apresentar os dados de forma mais amigável.

## Como fazer:

```Lua
-- Importar o módulo HTML
local html = require("html")

-- Definir a página web a ser analisada
local pagina = [[<html>
<head>
<title>Minha Página</title>
</head>
<body>
<h1>Bem-vindo à minha página</h1>
<p>Esta é uma página de exemplo</p>
</body>
</html>]]

-- Fazer parsing do código HTML
local dados = html.parse(pagina)

-- Acessar elementos específicos da página
local titulo = dados.head.title -- retorna "Minha Página"
local conteudo = dados.body.p -- retorna "Esta é uma página de exemplo"
```

## Mais informações:

O processo de parsing HTML é comumente usado na programação web para extrair dados específicos de páginas web. Ele também pode ser usado para validar a estrutura de uma página e identificar possíveis erros ou vulnerabilidades de segurança.

Existem várias alternativas para fazer parsing HTML, como o uso de expressões regulares ou o uso de bibliotecas específicas em outras linguagens de programação. No entanto, a função html.parse () do módulo HTML em Lua é uma opção eficiente e simples para realizar essa tarefa.

## Veja também:

Para saber mais sobre o processo de parsing HTML e como utilizá-lo em Lua, você pode consultar a documentação oficial do módulo HTML: https://github.com/zserge/lua-html

Além disso, você pode encontrar mais informações e exemplos sobre parsing HTML em Lua neste artigo do site Rosetta Code: http://rosettacode.org/wiki/HTML/Extract_data#Lua