---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:56:05.956453-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Verificar se um diretório existe é o ato de confirmar a presença de uma pasta no sistema de arquivos. Programadores fazem isso para evitar erros de arquivo não encontrado ou para decidir se criam ou não um novo diretório.

## Como Fazer:
```Fish Shell
if test -d /caminho/para/o/diretorio
    echo "O diretório existe!"
else
    echo "O diretório não existe."
end
```

Saída, se o diretório existir:
```
O diretório existe!
```

Saída, se o diretório não existir:
```
O diretório não existe.
```

## Mergulhando Fundo:
Em termos históricos, verificar diretórios é algo tão antigo quanto os próprios sistemas operacionais com suporte a files. No Fish Shell, o comando `test -d` é diretamente inspirado pelo utilitário de teste UNIX, consolidado desde os primeiros dias do sistema operacional. Alternativas a `test -d` incluem a execução de comandos como `ls` e interpretar o código de saída, mas isso é menos eficiente e pode produzir saída desnecessária. Implementações mais robustas podem envolver scripts Fish que lidam com permissões de leitura e escrita ou que integram log de atividades quando um diretório é verificado.

## Veja Também:
- Documentação oficial do comando `test` no Fish Shell: [https://fishshell.com/docs/current/commands.html#test](https://fishshell.com/docs/current/commands.html#test)
- Tutorial sobre scripting no Fish Shell: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
- FAQ do Fish Shell, que inclui algumas práticas recomendadas de uso: [https://fishshell.com/docs/current/faq.html](https://fishshell.com/docs/current/faq.html)
