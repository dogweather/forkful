---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?

Obter a data atual em qualquer linguagem de programação, incluindo Lua, é um aspecto básico no desenvolvimento de software. Isso é útil em situações onde o carimbo de data/hora é necessário também para o armazenamento e recuperação eficazes de dados.

## Como fazer!

Para obter a data atual em Lua, usamos a função *os.date()*, a qual devolve uma string formatada com a data e o tempo atual, quando não há argumento. Veja o exemplo abaixo:

```Lua
data_atual = os.date()
print("Data e Hora atual: " .. data_atual)
```

Quando você executa este código, o saída no console seria algo como:

```Lua
Data e Hora atual: Tue Sep 17 12:36:23 2022
```
## Mergulho Profundo

Historicamente, Lua foi concebido com interação fácil com softwares de C em mente, e assim, a função *os.date()* foi diretamente inspirada pela função homônima em biblioteca padrão de C. 

Existem também bibliotecas de terceiros, como date.lua ou lua-date que oferecem implementações de manipulação de data e hora mais avançadas e versáteis do que a biblioteca padrão.

Além disso, Se você precisar de um controle mais preciso sobre o formato da string de data, você pode passar um argumento de formato para a função *os.date()*. Por exemplo, para obter apenas a data no formato DD/MM/YYYY, você poderá fazer o seguinte:

```Lua
data_atual_formatada = os.date("%d/%m/%Y")
print("Data atual: " .. data_atual_formatada)
```

Isso imprimiria a data atual no formato mencionado, enquanto o símbolo de porcentagem (%) usado na string de formato é o prefixo necessário para todos os especificadores de formato em Lua.

## Veja Também

Aqui estão algumas referências úteis para você explorar mais sobre este tópico:

1. Documentação oficial Lua sobre o módulo os: https://www.lua.org/manual/5.4/manual.html#6.9

2. Documentação da função os.date(): http://www.lua.org/pil/22.1.html

3. date.lua, uma biblioteca de terceiros para manipulação de date e hora: https://github.com/Tieske/date

4. lua-date, uma biblioteca de data/hora para Lua bastante completa: https://github.com/torusjkl/lua-date