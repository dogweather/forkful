---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Concatenar strings, basicamente, significa juntar duas ou mais sequências de caracteres para formar uma string única. Os programadores fazem isso para manipular e gerenciar dados de textos de forma eficiente.

## Como fazer:
Aqui estão exemplos de como concatenar strings no Fish Shell:

```
set string1 "Olá, "
set string2 "mundo!"
set resultado $string1$string2
echo $resultado
```

A saída será:

```
Olá, mundo!
```

## Mergulho Profundo:
Historicamente, a concatenação de strings tem sido uma técnica importante na programação. Ela foi usada desde os primeiros dias do desenvolvimento de software, onde os programadores de baixo nível tinham que operar diretamente em strings para fazer coisas simples como imprimir mensagens na tela.

Existem alternativas para concatenar strings no Fish Shell e outros shells de linha de comando, dependendo do que você está tentando realizar. Por exemplo, você pode usar a função de concatenação especializada `string join` para unir strings com um delimitador específico:

```
set frutas "maçã" "banana" "cereja"
string join ", " $frutas
```

Em termos de implementação, a concatenação de strings no Fish Shell é um processo simples - as strings são simplesmente unidas na sequência em que aparecem. No entanto, é importante notar que ao lidar com strings muito longas ou um grande número de strings, a concatenação pode aumentar significativamente o uso de memória e a carga de computação.

## Veja Também:
Para mais informações, confira estes recursos úteis:

1. Documentação oficial do Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
2. Manual do Fish Shell na seção "String": [https://fishshell.com/docs/current/commands.html#string](https://fishshell.com/docs/current/commands.html#string)
3. Artigo sobre concatenação de strings: [https://en.wikipedia.org/wiki/Concatenation](https://en.wikipedia.org/wiki/Concatenation)
4. Stack Overflow para perguntas relacionadas: [https://stackoverflow.com/questions/tagged/fish](https://stackoverflow.com/questions/tagged/fish)