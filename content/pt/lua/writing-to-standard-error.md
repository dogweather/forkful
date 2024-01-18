---
title:                "Escrevendo para o erro padrão"
html_title:           "Lua: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# O que & Por que?

Escrever para o erro padrão (standard error) é uma técnica usada por programadores para imprimir mensagens de erro ou de depuração diretamente na linha de comando do programa, em vez de serem exibidas na saída padrão (standard output). Isso ajuda a distinguir claramente as mensagens de erro das mensagens de saída e facilita a depuração de problemas em um programa.

# Como fazer:

```Lua
-- Exemplo de escrita para o erro padrão
io.stderr:write("Houve um problema!")

-- Output:
Houve um problema!
```

É importante notar que a saída padrão e o erro padrão são canais de comunicação diferentes no programa. Enquanto a saída padrão é usada para imprimir dados e mensagens, o erro padrão é usado especificamente para mensagens de erro e depuração.

```Lua
-- Exemplo de uso de ambos os canais
print("Mensagem de saída")
io.stderr:write("Mensagem de erro")
```

É possível também formatar as mensagens de erro usando a função `string.format()` antes de enviá-las para o erro padrão.

```Lua
-- Exemplo de formatação de mensagem de erro
local x = 10
local y = "string"
io.stderr:write(string.format("Erro: %d não pode ser dividido por %s", x, y))

-- Output:
Erro: 10 não pode ser dividido por string
```

# Explorando mais a fundo:

Além de ser uma técnica útil para depurar problemas em um programa, escrever para o erro padrão também tem sua origem no Unix. Em sistemas Unix, geralmente é a saída padrão que é redirecionada para um arquivo ou outro processo, enquanto o erro padrão é exibido na tela. Isso permite que o usuário veja apenas as mensagens relevantes em tempo real.

Como alternativa ao uso de `io.stderr`, também é possível usar a função `error()` para lançar uma mensagem de erro diretamente da linha de comando.

```Lua
-- Exemplo de uso da função error()
error("Mensagem de erro!")

-- Output:
lua: mensagem de erro!
```

Ao usar `io.stderr:write()`, é importante lembrar de incluir a nova linha (`\n`) no final da mensagem para evitar que a próxima linha de comando ou mensagem seja colocada diretamente ao lado da mensagem de erro.

# Veja também:

- [Documentação Lua oficial - io.stderr](https://www.lua.org/manual/5.4/manual.html#pdf-io.stderr)
- [Artigo sobre gerenciamento de saída em Lua](https://www.lua.org/gems/sample.pdf)