---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O quê & Por quê?

Converter uma data para uma string é o processo de transformar um objeto de data em uma sequência de caracteres. Programadores fazem isso para tornar a data mais legível ou para usá-la em um contexto que exige uma string.

## Como fazer:

Aqui está um exemplo simples de como converter uma data para uma string no Lua:

```Lua
os.time()
data = os.date('*t')
dataFormatada = string.format('%02d/%02d/%04d', data.day, data.month, data.year)
print (dataFormatada)
```

E a seguir, o output do código acima será:

```Lua
09/07/2022
```

## Aprofundando:

- **Contexto histórico**: Lua foi criado em 1993 e desde então tem sido uma ferramenta útil para converter datas em strings. Isso sempre ajudou os programadores a manipular e apresentar datas de uma forma mais compreensível.
- **Alternativas**: Além do método `os.date`, Lua também oferece a função `os.time`, permitindo converter a data atual para um timestamp. No entanto, essa não retorna a data como string.
- **Detalhes de implementação**: Quando você chama a função `os.date` com o argumento `'*t'`, Lua retorna uma tabela representando a data e o tempo atuais. Depois, a função `string.format` é usada para formatar a data no formato desejado.

## Veja também:

- Documentação oficial do Lua: [https://www.lua.org/manual/5.4/manual.html](https://www.lua.org/manual/5.4/manual.html)
- String Format: [http://lua-users.org/wiki/StringLibraryTutorial](http://lua-users.org/wiki/StringLibraryTutorial)

Este artigo foi destinado a ser uma introdução rápida e sucinta ao tema de converter datas para strings no Lua. Para informações mais detalhadas, consulte a documentação oficial do Lua.