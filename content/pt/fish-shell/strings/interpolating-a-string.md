---
title:                "Interpolando uma string"
aliases:
- pt/fish-shell/interpolating-a-string.md
date:                  2024-01-20T17:50:58.052055-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Por Quê?
Interpolar uma string é o ato de inserir dinamicamente variáveis ou expressões dentro dela. Programadores fazem isso para construir mensagens ou comandos de forma flexível, diretamente no código, ajustando-se às diferentes situações e dados de entrada.

## Como Fazer:
```
# Defina uma variável
set name "Mundo"

# Interpole a variável dentro de uma string
echo "Olá, $name!"

# Saída esperada
Olá, Mundo!
```

```
# Utilize comandos em substituição de comandos (command substitution)
set greeting (echo "Olá")
echo "$greeting, $name!"

# Saída esperada
Olá, Mundo!
```

## Mergulho Profundo
A interpolação de strings no Fish Shell é direta e limpa, como visto acima. Historicamente, shells como o Bash exigiam uma sintaxe mais complexa, com uso de aspas e chaves de maneiras específicas. No Fish, a interpolação é facilitada pela sintaxe simples, que pode ser utilizada com variáveis e substituições de comandos.

Existem alternativas à interpolação, como a concatenação de strings, mas isso pode tornar o código mais verboso e difícil de ler. Pode-se dizer que a interpolação é uma forma de "template" simples, permitindo ao desenvolvedor manter a expressividade do código ao mesmo tempo que o mantém organizado e compreensível.

Internamente, quando você interpola uma string no Fish, o shell faz o processo de substituição antes de executar o comando, criando a versão final da string com todos os valores adequados no lugar.

## Veja Também
- Documentação oficial do Fish Shell sobre a sintaxe de variáveis: [fishshell.com/docs/current/index.html#variables](https://fishshell.com/docs/current/index.html#variables)
- Tutorial sobre substituição de comandos no Fish: [fishshell.com/docs/current/index.html#command-substitution](https://fishshell.com/docs/current/index.html#command-substitution)
