---
title:                "Gerando números aleatórios"
html_title:           "Fish Shell: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

É comum em muitos programas e scripts a necessidade de gerar números aleatórios para simular dados ou jogos, ou para realizar testes. A Fish Shell oferece recursos fáceis de usar para a geração de números aleatórios, tornando essa tarefa uma tarefa simples para os usuários.

## Como fazer

Para gerar um número aleatório no Fish Shell, basta utilizar o comando `math random`. Por exemplo:

```
Fish Shell> math random
0.472021
```

Você também pode especificar um intervalo de números para serem gerados. Por exemplo, se quisermos gerar um número entre 1 e 10, podemos usar o seguinte comando:

```
Fish Shell> math random 1 10
9
```

Podemos até mesmo gerar uma lista de números aleatórios usando o comando `math random-seed` juntamente com os comandos `for` e `echo`. Por exemplo:

```
Fish Shell> math random-seed 1234
Fish Shell> for i in (seq 5)
             echo (math random)
           end
0.966079
0.448646
0.700563
0.0292836
0.670263
```

## Mergulho profundo

A função `math random` utiliza o gerador de números pseudoaleatórios (PRNG) do seu sistema operacional, o que significa que os números gerados não são realmente aleatórios, mas sim determinados por uma fórmula matemática. Caso você precise de números verdadeiramente aleatórios, é possível utilizar ferramentas externas, como o comando `random` do pacote GNU coreutils, que gera números a partir de uma fonte de entropia no sistema.

Além disso, é possível especificar o formato de saída dos números gerados pelo comando `math random`, utilizando a opção `-F` seguida de um formato válido do printf do C. Por exemplo, se quisermos gerar um número com no máximo 2 casas decimais, podemos utilizar o comando:

```
Fish Shell> math random -F %.2f
0.63
```

## Veja também

- Documentação oficial da Fish Shell sobre o comando `math`: https://fishshell.com/docs/current/cmds/math.html
- Perguntas frequentes sobre o comando `math` no Fish Shell Wiki: https://github.com/fish-shell/fish-shell/wiki/Math-FAQ 
- Documentação do comando `random` do GNU coreutils: https://www.gnu.org/software/coreutils/manual/html_node/Any-Random-notes.html