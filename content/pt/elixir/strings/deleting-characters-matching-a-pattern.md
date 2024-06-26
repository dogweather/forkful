---
date: 2024-01-20 17:42:04.027764-07:00
description: "How to: Em Elixir, usamos principalmente a fun\xE7\xE3o `String.replace/4`\
  \ para deletar caracteres que correspondem a um padr\xE3o."
lastmod: '2024-03-13T22:44:46.222788-06:00'
model: gpt-4-1106-preview
summary: "Em Elixir, usamos principalmente a fun\xE7\xE3o `String.replace/4` para\
  \ deletar caracteres que correspondem a um padr\xE3o."
title: "Excluindo caracteres que correspondem a um padr\xE3o"
weight: 5
---

## How to:
Em Elixir, usamos principalmente a função `String.replace/4` para deletar caracteres que correspondem a um padrão.

```elixir
# Exemplo: Removendo todos os dígitos de uma string
string_original = "Elixir 1.11.3 é a versão mais recente!"
padrao = ~r/\d+/
substituicao = ""

String.replace(string_original, padrao, substituicao)
# Saída: "Elixir . é a versão mais recente!"

# Exemplo: Deletando todo espaçamento excessivo
string_com_espacos = " Elixir    é     incrível!  "
padrao_espacos = ~r/\s+/

String.replace(string_com_espacos, padrao_espacos, " ")
# Saída: " Elixir é incrível! "
```

## Deep Dive
Historicamente, manipulação de strings é uma tarefa regular na programação, e linguagens como Elixir, que têm DNA relacionado a Erlang e influências de linguagens funcionais, oferecem ferramentas eficazes. `String.replace/4` é versátil, permitindo substituição com base em padrões regex, o que torna o Elixir robusto para expressões regulares.

Alternativas incluem o uso de outras funções como `String.trim/1` para remover espaços no início e fim ou `String.slice/3` para cortar partes específicas, mas elas são menos flexíveis para padrões complexos.

Os detalhes de implementação envolvem a máquina virtual BEAM que executa Elixir. Ao compilar o padrão regex, o BEAM otimiza o desempenho da correspondência de padrões. Além disso, ser uma linguagem imutável significa que strings originais não são alteradas, garantindo a integridade dos dados.

## See Also
- [Documentação oficial de String](https://hexdocs.pm/elixir/String.html)
- [Expressões Regulares em Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Guia de RegEx](https://www.regular-expressions.info/)
- [Erlang's influence on Elixir](https://elixir-lang.org/crash-course.html#erlangs-influences)
