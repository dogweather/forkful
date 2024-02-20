---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:02.277461-07:00
description: "Capitalizar uma string em Bash envolve transformar o primeiro caractere\
  \ da string para mai\xFAscula, mantendo o resto da string inalterado. Esta t\xE9\
  cnica \xE9\u2026"
lastmod: 2024-02-19 22:05:05.788283
model: gpt-4-0125-preview
summary: "Capitalizar uma string em Bash envolve transformar o primeiro caractere\
  \ da string para mai\xFAscula, mantendo o resto da string inalterado. Esta t\xE9\
  cnica \xE9\u2026"
title: Capitalizando uma string
---

{{< edit_this_page >}}

## O Que & Por Que?
Capitalizar uma string em Bash envolve transformar o primeiro caractere da string para maiúscula, mantendo o resto da string inalterado. Esta técnica é comumente usada para formatar saídas ou cumprir com convenções de código que requerem que certas strings comecem com uma letra maiúscula para legibilidade ou preferências estilísticas.

## Como fazer:

O Bash não possui uma função embutida especificamente para capitalizar strings, mas você pode realizar essa tarefa usando expansão de parâmetro ou ferramentas externas como `awk`. Aqui estão algumas maneiras de capitalizar uma string em Bash:

**Usando Expansão de Parâmetro:**

Este método manipula a string diretamente no shell.

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
Saída:
```
Hello world
```

**Usando `awk`:**

`awk` é uma ferramenta poderosa de processamento de texto disponível na maioria dos sistemas operacionais do tipo Unix, que pode ser utilizada para capitalizar strings.

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
Saída:
```
Hello world
```

**Usando `sed`:**

Para uma abordagem mais tradicional, `sed` pode ser utilizado para capitalizar a primeira letra de uma string. No entanto, é um pouco mais complexo comparado aos métodos anteriores.

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
Saída:
```
Hello world
```

Estes trechos demonstram como capitalizar a primeira letra de uma string em Bash, destacando a flexibilidade da programação em shell ao manipular texto.
