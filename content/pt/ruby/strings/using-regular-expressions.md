---
aliases:
- /pt/ruby/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:07.664577-07:00
description: "Express\xF5es regulares (regex) em Ruby s\xE3o padr\xF5es usados para\
  \ combinar sequ\xEAncias de caracteres em strings, permitindo que desenvolvedores\
  \ pesquisem,\u2026"
lastmod: 2024-02-18 23:08:58.653896
model: gpt-4-0125-preview
summary: "Express\xF5es regulares (regex) em Ruby s\xE3o padr\xF5es usados para combinar\
  \ sequ\xEAncias de caracteres em strings, permitindo que desenvolvedores pesquisem,\u2026"
title: "Usando express\xF5es regulares"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Expressões regulares (regex) em Ruby são padrões usados para combinar sequências de caracteres em strings, permitindo que desenvolvedores pesquisem, correspondam e manipulem texto de maneira eficiente. Programadores utilizam regex para tarefas como validação, análise (parsing) e manipulação de strings, tornando-o uma ferramenta indispensável para o processamento de texto.

## Como fazer:
### Correspondência Básica
Para combinar uma string com um padrão simples, você pode usar o método `match`. Abaixo, estamos verificando se a palavra "Ruby" existe em uma string dada.

```ruby
if /Ruby/.match("Olá, Ruby!")
  puts "Combinação encontrada!"
end
# Saída: Combinação encontrada!
```

### Correspondência de Padrões com Variáveis
Você pode interpolar variáveis em sua regex usando a sintaxe `#{}`, tornando seus padrões dinâmicos.

```ruby
language = "Ruby"
if /#{language}/.match("Programar em Ruby é divertido.")
  puts "Falando sobre Ruby!"
end
# Saída: Falando sobre Ruby!
```

### Usando Regex para Substituição
O método `gsub` permite substituir toda ocorrência de um padrão por uma string de substituição especificada.

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# Saída: barbarbar
```

### Captura
Parênteses em uma regex são usados para capturar partes de uma combinação. O método `match` retorna um objeto `MatchData`, que você pode usar para acessar as capturas.

```ruby
match_data = /(\w+): (\d+)/.match("Idade: 30")
puts match_data[1] # Rótulo capturado
puts match_data[2] # Valor capturado
# Saída:
# Idade
# 30
```

### Usando Bibliotecas de Terceiros
Embora a biblioteca padrão do Ruby seja poderosa, às vezes você pode precisar de funcionalidades mais especializadas. Uma gem popular para trabalhar com regex é `Oniguruma`, que fornece recursos adicionais de regex além do motor de regex integrado ao Ruby.

Instale-a usando:
```bash
gem install oniguruma
```

Um exemplo de uso poderia ser assim (assumindo que você tenha feito o require de `oniguruma` após instalá-la):

```ruby
# Este é um exemplo mais avançado e pode requerer configuração adicional
require 'oniguruma'

padrão = Oniguruma::ORegexp.new('(\d+)')
dados_da_combinação = padrão.match("O número é 42.")
puts dados_da_combinação[1]
# Saída: 42
```

Lembre-se, embora poderosas, expressões regulares podem se tornar complexas e difíceis de gerenciar para padrões mais complicados. Vise a legibilidade e considere métodos alternativos se sua regex se tornar muito intrincada.
