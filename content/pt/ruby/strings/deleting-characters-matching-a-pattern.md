---
title:                "Excluindo caracteres que correspondem a um padrão"
aliases: - /pt/ruby/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:10.438361-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Eliminar caracteres que correspondem a um padrão é simplesmente a arte de selecionar certos trechos de uma string baseado em regras e dizer adeus a eles. Programadores fazem isso para limpar e formatar strings, seja removendo caracteres desnecessários ou extraindo informação relevante.

## Como fazer:

```Ruby
# Remove todos os dígitos de uma string
string = "Preço: R$123,99."
limpa_string = string.gsub(/[0-9]/, '')
puts limpa_string
# Saída: Preço: R$,.

# Remove tudo exceto letras
string_com_caracteres = "O #Ruby é 100% incrível!"
apenas_letras = string_com_caracteres.gsub(/[^a-zA-Záéíóúãõàç ]/, '')
puts apenas_letras
# Saída: O Ruby é incrível

# Deleta caracteres especificos
frase = "Olá, mundo!"
frase_sem_vogais = frase.delete('aeiou')
puts frase_sem_vogais
# Saída: Ol, mnd!
```

## Mergulho Profundo:

A função de eliminação de caracteres remonta ao início da programação. Em Ruby, métodos como `delete` e `gsub` são comumente usados para essa tarefa. O `delete` é direto e elimina todos os caracteres listados, enquanto o `gsub` permite substituir padrões definidos por regex (expressões regulares) por outra coisa - se você substituir por uma string vazia, é equivalente a deletar.

Existem alternativas além do Ruby: linguagens como Python têm métodos como `replace` e `re.sub`, enquanto o JavaScript utiliza `replace` com regex. Em termos de desempenho e implementação, saber o que está acontecendo por baixo dos panos ajuda. No Ruby, por exemplo, `gsub` é implementado em C, o que o torna bastante rápido para strings pequenas a médias, mas sempre fique atento ao trabalhar com strings muito grandes e operações complexas de regex, pois podem ser custosas em termos de tempo de execução.

## Veja também:

- [Ruby-doc: String](https://ruby-doc.org/core-3.1.0/String.html) - documentação oficial de Strings no Ruby.
- [RegExr](https://regexr.com/) - ferramenta para aprender e testar expressões regulares.
- [Rubular](http://rubular.com/) - um editor de regex Ruby para testar suas expressões regulares rapidamente.
