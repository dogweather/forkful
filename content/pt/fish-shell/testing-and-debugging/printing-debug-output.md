---
title:                "Exibindo saídas de depuração"
aliases:
- /pt/fish-shell/printing-debug-output/
date:                  2024-01-20T17:52:43.651320-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
A impressão de saídas de depuração ajuda a entender o que um script está fazendo e onde pode estar o problema. Programadores recorrem a isso quando o código não funciona como esperado, para visualizar o que está acontecendo "sob o capó".

## Como Fazer:
Vou te mostrar como imprimir mensagens para depurar teu código em Fish. Simples e rápido.

```Fish Shell
# Imprime uma mensagem na saída padrão
echo "Debug: Valor da variável é" $minha_variavel

# Executa um comando e imprime seu resultado
set output (comando_aqui)
echo "Debug: O resultado foi" $output

# Para mostrar a mensagem de erro, redireciona a saída padrão para a saída de erro
echo "Debug: Ops, algo deu errado" >&2
```

Saída possível após a execução:
```
Debug: Valor da variável é 42
Debug: O resultado foi sucesso
Debug: Ops, algo deu errado
```

## Mergulho Profundo:
Lá nos tempos antigos do Unix, a depuração já era uma necessidade. Usávamos ferramentas como `printf` para C ou o comando `echo` nos primeiros shells. No Fish, continuamos usando `echo`, mas com algumas vantagens. O Fish possui sintaxe simplificada e colorização automática que facilita na hora de separar as saídas de depuração do resto. Além disso, redirecionamentos como `>&2` são intuitivos: você está dizendo pro shell "ei, manda isso aqui pra saída de erro, por favor". 

Como alternativa ao método clássico, podemos também usar funções específicas do Fish, como `printf`, que dá mais controle sobre o formato da saída. E, para os fãs do silêncio, um truque é usar `set -l` ou `set -g` para criar variáveis temporárias, passando informações sem poluir a saída.

A implementação desse recurso é direta: o comando de eco é parte do próprio shell e trabalha junto com o sistema de redirecionamento de fluxo do Unix, que permite a especificação de pra onde vão as mensagens — seja pra tela do usuário, seja pra um arquivo ou pra outro programa.

## Veja Também:
- [Documentação Oficial do Fish sobre Echo](https://fishshell.com/docs/current/cmds/echo.html)
- [Fórum de Discussão do Fish](https://fishshell.com/docs/current/faq.html)
