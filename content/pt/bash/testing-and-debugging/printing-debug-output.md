---
title:                "Exibindo saídas de depuração"
aliases: - /pt/bash/printing-debug-output.md
date:                  2024-01-20T17:51:53.599532-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Imprimir saídas de debug é basicamente escrever informações sobre o que está acontecendo dentro do código enquanto ele é executado. Programadores fazem isso para entender e resolver problemas – sim, somos detetives digitais.

## Como Fazer:

Para imprimir algo na tela, use `echo` ou `printf`. O `echo` é simples e direto, enquanto `printf` é mais flexível para formatar a saída.

```Bash
# Usando echo
echo "Algo está acontecendo aqui..."

# Usando printf
printf "Valor de X: %d\n" $X
```

Resultado esperado:

```Bash
Algo está acontecendo aqui...
Valor de X: 42
```

Adicione `-e` no `echo` para interpretar caracteres especiais e inclua `>` ou `>>` para redirecionar a saída para um arquivo.

```Bash
# Echo com caracteres especiais
echo -e "Uma linha.\nOutra linha."

# Redirecionando para um arquivo
echo "Isso vai para um arquivo" > debug.log
```

Resultado esperado:

```Bash
Uma linha.
Outra linha.
# E o conteúdo é adicionado ao arquivo debug.log
```

## Aprofundando:

O `echo` já existe desde os primórdios dos shells Unix. O `printf`, por sua vez, vem da linguagem C e oferece mais controle sobre o formato. Há também os comandos `stderr` e `stdout` que direcionam para saídas de erro e saída padrão, respectivamente, que podem ser úteis para separar logs de erro dos normais.

Alternativas incluem o uso de ferramentas como o `tee` para escrever em múltiplos destinos e o uso de frameworks de logging que oferecem mais complexidade e controle para aplicações grandes.

Quando falar de implementação, certifique-se de limpar ou comentar os `echo` ou `printf` que usou para debug antes de passar o código para produção. Isso mantém o código limpo e evita vazamento de informação sensível.

## Veja Também:

- A [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/) fornece uma visão mais detalhada sobre o scripting em Bash.
- A documentação oficial do [GNU Bash](https://www.gnu.org/software/bash/manual/) é útil para entender todas as funcionalidades disponíveis.
- Para boas práticas de desenvolvimento, o [Google's Shell Style Guide](https://google.github.io/styleguide/shellguide.html) é um ótimo ponto de partida.
