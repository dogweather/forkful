---
date: 2024-01-20 17:50:45.751851-07:00
description: "Como Fazer: Interpolar strings \xE9 direto. Use `#{}` para encaixar\
  \ vari\xE1veis ou c\xF3digos no meio do texto."
lastmod: '2024-04-05T21:53:46.551094-06:00'
model: gpt-4-1106-preview
summary: "Interpolar strings \xE9 direto."
title: Interpolando uma string
weight: 8
---

## Como Fazer:
```elixir
nome = "João"
idade = 29

# Interpolação básica com #{}
mensagem = "Olá, #{nome}, você tem #{idade} anos."
IO.puts(mensagem)

# Output:
# Olá, João, você tem 29 anos.
```

Interpolar strings é direto. Use `#{}` para encaixar variáveis ou códigos no meio do texto.

```elixir
# Expressões dentro da interpolação
comprimento = 15
largura = 10
mensagem_area = "A área do retângulo é #{comprimento * largura} metros quadrados."
IO.puts(mensagem_area)

# Output:
# A área do retângulo é 150 metros quadrados.
```

## Aprofundamento
Antes de Elixir, outras linguagens como Ruby e Perl já suportavam interpolação de strings. A interpolação em Elixir é feita em tempo de compilação, o que significa que é transformada em código executável juntamente com o restante do programa. Isso traz eficiência, já que não há processamento extra em tempo de execução.

Alternativas à interpolação incluem concatenação manual de strings com o operador `<>`, mas isso pode tornar o código mais difícil de ler e manter.

```elixir
# Concatenação manual
mensagem_concatenada = "Olá, " <> nome <> ", você tem " <> Integer.to_string(idade) <> " anos."
IO.puts(mensagem_concatenada)
```

No entanto, a interpolação só pode ser usada dentro de strings delimitadas por aspas duplas. Aspas simples criam sigils que não suportam interpolação.

Elixir também oferece a capacidade de transformar qualquer tipo de dado em uma string dentro do bloco de interpolação, chamando automaticamente a função `to_string` se necessário, o que facilita a integração de diferentes tipos de dados.

## Veja Também
- Documentação oficial do Elixir sobre Strings: [https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- Um tutorial mais profundo sobre manipulação de Strings em Elixir: [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)
