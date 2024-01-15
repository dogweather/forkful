---
title:                "Gerando números aleatórios"
html_title:           "Bash: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que

Gerar números aleatórios é uma tarefa bastante comum em diversas áreas da programação. Pode ser necessário para criar senhas seguras, testar algoritmos, simular cenários e muito mais.

## Como fazer

Para gerar números aleatórios em Bash, podemos utilizar o comando "shuf". Ele permite gerar números aleatórios ou embaralhar linhas de um arquivo. Veja um exemplo:

```Bash
# Gera 5 números aleatórios entre 1 e 10
shuf -i 1-10 -n 5
# Saída: 7 2 5 1 9

# Embaralha as linhas de um arquivo
shuf arquivo.txt
# Saída: linha3 linha1 linha2 
```
## Mais detalhes

O comando "shuf" utiliza um gerador de números aleatórios baseado no algoritmo Mersenne Twister. Ele é considerado um dos mais eficientes e confiáveis para gerar sequências aleatórias.

É importante notar que o comando "shuf" não gera números verdadeiramente aleatórios. Ele utiliza uma semente (seed) para a geração dos números, o que significa que se a mesma semente for utilizada em momentos diferentes, a sequência de números gerados será sempre a mesma. 

Podemos alterar a semente utilizada pelo comando, adicionando o argumento "-r" e um número inteiro após a opção "shuf". Por exemplo:

```Bash
shuf -r 123456 arquivo.txt
```

Isso irá gerar uma sequência diferente de números aleatórios a cada vez que o comando for executado. 

Além disso, é possível limitar o intervalo de números gerados utilizando as opções "-i" (intervalo) e "-n" (quantidade de números gerados).

Com essas informações, é possível utilizar o comando "shuf" de diversas formas para gerar números aleatórios de acordo com suas necessidades.

## Veja também

- Documentação oficial do "shuf": https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html
- Mais sobre o algoritmo Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister
- Outras formas de gerar números aleatórios em Bash: https://linuxhint.com/generate_random_numbers_bash/