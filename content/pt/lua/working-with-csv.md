---
title:                "Trabalhando com arquivos csv"
html_title:           "Lua: Trabalhando com arquivos csv"
simple_title:         "Trabalhando com arquivos csv"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## O que é e por quê?
Programar com CSV (Comma-Separated Values) significa lidar com dados em formato de tabela separados por vírgulas. Programadores usam isso para armazenar, manipular e analisar grandes conjuntos de dados de forma organizada e eficiente.

## Como fazer:
Como exemplo, vamos supor que temos um arquivo CSV com informações sobre clientes de uma empresa. Podemos ler e imprimir esses dados da seguinte forma:

```Lua
-- Abrindo o arquivo e lendo os dados
file = io.open("clientes.csv", "r")
data = file:read("*all")

-- Separando os campos por vírgula e imprimindo cada cliente
for name, email in data:gmatch("(%a+,%a+@%a+.%a+)") do
  print(name, email)
end
```

A saída seria algo como:
```
Maria,maria@example.com
João,joao@example.com
Ana,ana@example.com
```

## Mergulho Profundo:
CSV foi criado nos anos 1970 como uma forma simples de armazenar grandes quantidades de dados tabulares sem a necessidade de um banco de dados. Hoje em dia, existem alternativas mais avançadas, como JSON e XML, mas CSV ainda é amplamente utilizado por sua facilidade de uso e compatibilidade com diferentes softwares.

Para lidar com arquivos CSV no Lua, é importante importar uma biblioteca como a `lunajson` ou `lua-csv` para auxiliar na manipulação dos dados. Além disso, é importante lembrar de sempre tratar possíveis erros de formatação ou dados ausentes.

## Veja também:
- [Documentação oficial do Lua](https://www.lua.org/docs.html)
- [Biblioteca lua-csv](https://github.com/geoffleyland/lua-csv)