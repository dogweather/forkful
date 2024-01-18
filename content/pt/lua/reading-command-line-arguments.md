---
title:                "Lendo argumentos da linha de comando"
html_title:           "Lua: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# O que e por que?

Ler argumentos da linha de comando em Lua significa obter informações inseridas através do terminal ao executar um programa. Isso é importante para programadores, pois permite que eles capturem e utilizem dados do usuário no seu código.

# Como fazer:

```lua
-- O argumento a seguir é inserido após o nome do arquivo Lua no terminal
local argumento = arg[1] 
print("Seu argumento foi: " .. argumento)
```

**Exemplo de saída:**
```
Seu argumento foi: argumento_inserido_pelo_usuario
```

# Profundidade:

- Contexto histórico:
Ler argumentos da linha de comando é uma técnica que existe desde os primeiros sistemas operacionais de linha de comando. Foi originalmente desenvolvido para permitir que os usuários personalizassem a execução de programas com diferentes configurações e opções.

- Alternativas:
Uma alternativa para ler argumentos da linha de comando em Lua é usando a biblioteca `argparse`, que oferece uma maneira mais sofisticada de gerenciar e analisar argumentos de linha de comando.

- Detalhes de implementação:
Ao executar um programa Lua, os argumentos da linha de comando são armazenados em uma tabela chamada `arg` e podem ser acessados ​​utilizando índices numéricos. O primeiro argumento é armazenado no índice 1, o segundo no índice 2, e assim por diante.

# Veja também:

- [Documentação oficial do Lua sobre os argumentos da linha de comando](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Biblioteca `argparse` para análise de argumentos de linha de comando em Lua](https://github.com/mpeterv/argparse)