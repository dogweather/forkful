---
title:    "Fish Shell: Convertendo uma string para letras minúsculas"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que
O uso de letras maiúsculas e minúsculas é um aspecto importante da linguagem de programação. Converter todas as letras de uma string para minúsculas pode ser útil para fins de comparação de dados ou para garantir que a entrada do usuário seja padronizada. Neste blog post, vamos explorar como é possível converter uma string para letras minúsculas usando o Fish Shell.

## Como fazer
Abaixo está um exemplo de código que mostra como converter uma string para letras minúsculas usando o Fish Shell. Este código foi testado na versão 3.1.2 do Fish Shell.

```
# Definir a string original
set original_string "Olá MUNDO"

# Converter a string para letras minúsculas
set lower_string (string tolower $original_string)

# Imprimir a string resultante
echo $lower_string
```

O output deste código será "olá mundo", que é a string original convertida para letras minúsculas. O comando "string tolower" é usado para realizar a conversão, e a variável "$original_string" é usada para armazenar e fornecer a string a ser convertida.

O Fish Shell também possui uma função embutida para converter diretamente strings para letras minúsculas, chamada "lower" (ou "to-lower" na versão mais antiga do shell). Veja o exemplo abaixo:

```
set original_string "Olá MUNDO"
set lower_string (lower $original_string)
echo $lower_string
```

O output será o mesmo que no exemplo anterior: "olá mundo".

É importante observar que esse método de conversão não faz distinção entre caracteres acentuados e não acentuados, ou seja, todos os caracteres serão transformados em minúsculos, incluindo os acentuados. Se você precisar preservar os acentos, pode seguir uma abordagem diferente, usando a função "from-string" do Fish Shell para gerar uma tabela de conversão personalizada.

## Profundidade
A função "string tolower" é capaz de converter não apenas strings, mas também variáveis, listas e até comandos. Isso significa que você pode usar a função em diferentes contextos para obter resultados interessantes.

Por exemplo, você pode usar a função dentro de um loop para converter todos os itens de uma lista para letras minúsculas. Ou pode usá-la para converter a saída de um comando diretamente para letras minúsculas, sem a necessidade de armazenar a saída em uma variável.

A função "to-lower" também pode ser combinada com outras funções do Fish Shell, como "string subst" (para substituir partes específicas da string) ou "string split" (para dividir a string em sub-strings). Isso permite uma maior flexibilidade na manipulação de strings em minúsculas.

## Veja também
- Documentação do Fish Shell sobre a função to-lower (https://fishshell.com/docs/current/cmds/to-lower.html)
- Guia oficial do Fish Shell (https://fishshell.com/docs/current/)
- Fórum oficial do Fish Shell (https://www.reddit.com/r/fishshell/)