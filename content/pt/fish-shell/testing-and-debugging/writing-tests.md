---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:32.516530-07:00
description: "Como: Fish n\xE3o possui uma estrutura de teste integrada como alguns\
  \ outros ambientes de programa\xE7\xE3o. No entanto, voc\xEA pode escrever scripts\
  \ de teste\u2026"
lastmod: '2024-03-13T22:44:47.009519-06:00'
model: gpt-4-0125-preview
summary: "Fish n\xE3o possui uma estrutura de teste integrada como alguns outros ambientes\
  \ de programa\xE7\xE3o."
title: Escrevendo testes
weight: 36
---

## Como:
Fish não possui uma estrutura de teste integrada como alguns outros ambientes de programação. No entanto, você pode escrever scripts de teste simples que usam asserções para verificar o comportamento de suas funções. Além disso, você pode aproveitar ferramentas de terceiros como `fishtape` para um conjunto de testes mais abrangente.

### Exemplo 1: Script de Teste Básico
Vamos começar com uma função básica em Fish que calcula a soma de dois números:

```fish
function add --description 'Adiciona dois números'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

Você pode escrever um script de teste básico para essa função assim:

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add aprovado"
    else
        echo "test_add reprovado"
    end
end

test_add
```

Executar este script produziria:

```
test_add aprovado
```

### Exemplo 2: Usando Fishtape
Para uma solução de teste mais robusta, você pode usar `fishtape`, um executor de teste que produz TAP para Fish.

Primeiro, instale `fishtape`, se ainda não o fez:

```fish
fisher install jorgebucaran/fishtape
```

Em seguida, crie um arquivo de teste para a sua função `add`, por exemplo, `add_test.fish`:

```fish
test "Adicionar 3 e 4 resulta em 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

Para executar o teste, use o seguinte comando:

```fish
fishtape add_test.fish
```

A saída de exemplo pode parecer com:

```
TAP version 13
# Adicionar 3 e 4 resulta em 7
ok 1 - test_add aprovado
```

Isso indica que o teste passou com sucesso. `fishtape` permite que você estruture testes mais detalhados e fornece uma saída informativa, facilitando a depuração e cobertura de teste abrangente para seus scripts Fish.
