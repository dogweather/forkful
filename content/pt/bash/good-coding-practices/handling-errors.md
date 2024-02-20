---
date: 2024-01-26 00:50:21.970698-07:00
description: "Tratar erros em scripts Bash \xE9 antecipar onde as coisas podem dar\
  \ errado e lidar com isso de maneira elegante. Por qu\xEA? Bom, isso mant\xE9m seu\
  \ script\u2026"
lastmod: 2024-02-19 22:05:05.815422
model: gpt-4-1106-preview
summary: "Tratar erros em scripts Bash \xE9 antecipar onde as coisas podem dar errado\
  \ e lidar com isso de maneira elegante. Por qu\xEA? Bom, isso mant\xE9m seu script\u2026"
title: Tratamento de erros
---

{{< edit_this_page >}}

## O Que & Por Quê?

Tratar erros em scripts Bash é antecipar onde as coisas podem dar errado e lidar com isso de maneira elegante. Por quê? Bom, isso mantém seu script robusto e poupa os usuários de quebra-cabeças quando as coisas não funcionam como esperado.

## Como fazer:

```Bash
#!/bin/bash

# Redirecionando stderr para um arquivo
grep "algo" arquivo.txt 2> erros.log

# Tratamento de erro com status de saída
if ! grep "algo" arquivo.txt; then
    echo "Ops, algo deu errado ao procurar por 'algo'."
    exit 1
fi

# Usando um trap para limpar antes de sair por erro
cleanup() {
  echo "Limpando arquivos temporários..."
  rm temp_*
}

trap cleanup ERR

# erro intencional: arquivo não existe
cat temp_file.txt
```

Saída de exemplo quando ocorre um erro:

```
Limpando arquivos temporários...
cat: temp_file.txt: Arquivo ou diretório não encontrado
```

## Aprofundamento

O tratamento de erros em scripts Bash remonta às origens do shell Unix, onde scripts robustos e confiáveis eram (e são) vitais para administração de sistemas e automação. Tradicionalmente, erros em Bash são tratados verificando o status de saída de um comando, que por convenção retorna 0 para sucesso e um valor não zero para falha.

O Bash introduziu o comando `trap` como um built-in, permitindo que os usuários especifiquem comandos para rodar em vários sinais ou saídas de script. Isso é útil para tarefas de limpeza ou como um mecanismo de tratamento de erros de último recurso.

Há também o comando `set`, que pode mudar o comportamento do Bash em erros. Por exemplo, `set -e` fará com que um script saia imediatamente se qualquer comando sair com um status não zero, uma maneira de falhar rapidamente e evitar erros em cascata.

Alternativas ao tratamento de erro interno do Bash incluem a verificação explícita da existência de arquivos, o uso de substituição de comandos ou até mesmo a escrita de suas próprias funções para lidar com erros de forma mais granular.

Embora o tratamento de erros rigoroso possa às vezes parecer exagero para scripts pequenos, é uma prática que pode economizar muito tempo de depuração e prevenir comportamentos inesperados tanto para você quanto para os usuários.

## Veja Também

- Manual do Bash sobre Parâmetros de Shell: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Guia de Scripting Bash Avançado sobre Tratamento de Erros: https://www.tldp.org/LDP/abs/html/exit-status.html
- Um guia aprofundado sobre `trap`: https://mywiki.wooledge.org/SignalTrap

Lembre-se, scripting é uma forma de arte, e como você lida com deslizes e tropeços pode tornar sua obra-prima mais resiliente. Feliz scriptagem!
