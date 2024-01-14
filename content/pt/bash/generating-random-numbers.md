---
title:    "Bash: Gerando números aleatórios"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

Ao escrever scripts e programas em Bash, pode ser útil gerar números aleatórios para uma variedade de tarefas. Isso inclui desde selecionar aleatoriamente um item de uma lista até simular jogos de azar. Gerar números aleatórios também pode ser útil em testes de software, onde é necessário criar conjuntos de dados aleatórios para verificar a robustez de um sistema.

## Como fazer?

Para gerar números aleatórios em Bash, podemos usar a função `echo` juntamente com o `RANDOM` builtin. Por exemplo, para gerar um número aleatório entre 1 e 10, podemos usar o seguinte código:

```Bash
echo $((RANDOM%10+1))
```

Isso irá exibir um número aleatório entre 1 e 10 toda vez que o script for executado. Para gerar um número aleatório com uma maior faixa, basta ajustar os valores no código acima.

Podemos até mesmo usar o `RANDOM` builtin para preencher variáveis com valores aleatórios. Por exemplo, para gerar uma senha de 8 dígitos, podemos usar o seguinte código:

```Bash
password=$RANDOM$RANDOM$RANDOM
echo ${password:0:8}
```

Esse código irá gerar uma senha aleatória com 8 dígitos sempre que o script for executado.

## Profundidade na geração de números aleatórios

Por baixo dos panos, a função `echo` em conjunto com o `RANDOM` builtin utiliza o gerador de números pseudoaleatórios (PRNG) do sistema operacional. Esse gerador utiliza um algoritmo complexo para produzir números aparentemente aleatórios a partir de uma semente predefinida. A semente é gerada automaticamente pelo sistema operacional a cada inicialização, garantindo que o PRNG produza sequências diferentes a cada execução.

É importante notar que, apesar de parecerem aleatórios, os números gerados pelo PRNG são previsíveis e, portanto, não devem ser usados em situações que exigem alta segurança, como na geração de senhas para sistemas críticos.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Perguntas frequentes sobre geração de números aleatórios no Bash](http://mywiki.wooledge.org/BashFAQ/026)
- [Artigo sobre o gerador de números pseudoaleatórios no Linux](https://www.2uo.de/myths-about-urandom/)