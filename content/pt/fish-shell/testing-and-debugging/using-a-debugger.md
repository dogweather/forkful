---
date: 2024-01-26 03:48:41.528340-07:00
description: "Como fazer: O Fish n\xE3o possui um depurador embutido como alguns outros\
  \ shells, mas voc\xEA pode usar ferramentas externas como `gdb` para depurar programas\u2026"
lastmod: '2024-03-13T22:44:47.010484-06:00'
model: gpt-4-0125-preview
summary: "O Fish n\xE3o possui um depurador embutido como alguns outros shells, mas\
  \ voc\xEA pode usar ferramentas externas como `gdb` para depurar programas compilados\
  \ ou `fish -d` para executar o fish com sa\xEDda de depura\xE7\xE3o em diferentes\
  \ n\xEDveis."
title: Usando um depurador
weight: 35
---

## Como fazer:
O Fish não possui um depurador embutido como alguns outros shells, mas você pode usar ferramentas externas como `gdb` para depurar programas compilados ou `fish -d` para executar o fish com saída de depuração em diferentes níveis. Vamos prosseguir com `fish -d`:

```fish
# Executa o shell fish com nível de depuração 2
fish -d2

# No shell fish, vamos testar uma função simples com um potencial erro
function test_func
    set val 42
    echo "O valor é $val"
    if test $val -eq 42
        echo "Tudo está bem."
    else
        echo "Algo está estranho."
    end
end

# Chame a função e observe a saída de depuração
test_func
```

Você veria uma saída de depuração extra antes e depois da execução da função, ajudando a identificar problemas.

## Mergulho Profundo
Historicamente, a depuração em ambientes similares ao Unix tem sido uma província de ferramentas especializadas como `gdb` para C/C++ ou `pdb` para Python. No Fish, geralmente depende-se de utilitários externos ou recursos internos como `functions -v` para saída detalhada de funções e `set -x` para rastrear mudanças nas variáveis.

Algumas pessoas escolhem shells alternativos como o Bash por conta de recursos como `set -x` para depurar scripts. No entanto, o Fish tem seu charme com um foco na facilidade de uso e interatividade, o que pode reduzir a necessidade de depuração intensa em muitos casos.

Quando se trata de implementação, depurar um script geralmente envolve executá-lo com saída detalhada e rastrear onde as variáveis são definidas, desfeitas ou mutadas de maneiras inesperadas. Com a saída colorida do Fish e abordagem amigável ao usuário, muitas vezes você pode evitar a parte difícil da depuração – mas quando estiver preso, lembre-se de que a verbosidade e a clareza são suas melhores ferramentas.

## Veja Também
Aqui estão algumas linhas de vida confiáveis quando você está até as barbatanas em código:

- Documentação do Fish sobre depuração: https://fishshell.com/docs/current/index.html#debugging
- Guia oficial do GDB (GNU Debugger): https://www.gnu.org/software/gdb/documentation/
- Tag Fish no Stack Overflow - casos de depuração no mundo real: https://stackoverflow.com/questions/tagged/fish
- Guia Avançado de Scripting em Bash - para comparar abordagens de depuração: https://tldp.org/LDP/abs/html/debugging.html
