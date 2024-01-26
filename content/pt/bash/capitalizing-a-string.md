---
title:                "Capitalizando uma string"
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Capitalizar uma string é transformar todas as suas letras em maiúsculas. Fazemos isso para normalizar dados, criar ênfase ou atender a requisitos de formatação.

## Como fazer:
Aqui estão alguns exemplos simples de como capitalizar strings em Bash:

```Bash
#!/bin/bash
string_min="vamos capitalizar!"
string_mai=$(echo "$string_min" | tr '[:lower:]' '[:upper:]')
echo $string_mai
```

Saída:
```
VAMOS CAPITALIZAR!
```

Outra forma, usando o comando `awk`:
```Bash
#!/bin/bash
string_min="aprendendo com diversão"
echo $string_min | awk '{print toupper($0)}'
```

Saída:
```
APRENDENDO COM DIVERSÃO
```

E se você quiser apenas a primeira letra maiúscula:
```Bash
#!/bin/bash
string_min="olá, mundo!"
string_cap="$(tr '[:lower:]' '[:upper:]' <<< ${string_min:0:1})${string_min:1}"
echo $string_cap
```

Saída:
```
Olá, mundo!
```

## Aprofundamento
Originalmente, capitalizar strings em computadores era importante porque os primeiros sistemas eram case-sensitive e, às vezes, apenas reconheciam maiúsculas. Questões legadas persistem, e por isso ainda hoje a capitalização é importante em programação.

Alternativamente, em scripts Bash modernos, você pode usar expansões de parâmetro do Bash para capitalizar strings sem chamar utilitários externos, como `tr` ou `awk`. No entanto, essas expansões de shell são um tanto mais avançadas e podem não ser portáveis para todas as versões do shell.

Detalhes de implementação a serem observados são que o comando `tr` funciona por substituição de caracteres e é muito rápido para strings longas, enquanto o `awk` é uma ferramenta de manipulação de texto mais poderosa e flexível, capaz de executar uma grande variedade de tarefas.

## Veja também
- [AWK Command in Unix/Linux with Examples](https://www.geeksforgeeks.org/awk-command-unixlinux-examples/)
