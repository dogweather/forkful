---
title:                "Bash: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Por que gerar números aleatórios em programação Bash?

Ao criar programas em Bash, é importante ter a capacidade de gerar números aleatórios. Isso pode ser útil em diversos cenários, como jogos, sorteios ou até mesmo para testar algoritmos e lógica de programação. Gerar números aleatórios é uma habilidade essencial para qualquer programador Bash.

## Como fazer isso?

Felizmente, o Bash já possui uma função embutida para gerar números aleatórios, chamada "RANDOM". Basta usar os comandos "echo" e "$RANDOM" em conjunto para exibir um número aleatório na tela. Por exemplo:

```Bash
echo $RANDOM
```

Isso irá gerar um número aleatório entre 0 e 32767 sempre que o comando for executado. Você também pode especificar um limite máximo para o número gerado usando a seguinte fórmula:

```Bash
echo $((RANDOM%MAX +1 ))
```

Substitua "MAX" pelo valor máximo desejado. Por exemplo, se quiser gerar um número entre 1 e 10, o comando ficaria assim:

```Bash
echo $((RANDOM%10 +1 ))
```

Você também pode usar esses comandos em um loop para gerar múltiplos números aleatórios. Por exemplo, o seguinte código irá gerar e exibir 5 números aleatórios entre 1 e 100:

```Bash
#!/bin/bash

for i in {1..5}
do
    echo $((RANDOM%100 +1 ))
done
```

Exemplo de saída:

```
63
92
47
3
88
```

## Aprofundando-se

Para gerar números verdadeiramente aleatórios, é necessário utilizar ferramentas externas ao Bash, como o comando "shuf" ou o programa "random". Essas ferramentas utilizam algoritmos complexos para gerar sequências de números aleatórios sem repetições. Você também pode utilizar tecnologias como o gerador de números aleatórios baseado em hardware (HWRNG) ou até mesmo sites externos especializados em geração de números aleatórios.

Uma opção comumente usada é o comando "shuf", que mistura um conjunto de números em ordem aleatória. Por exemplo:

```Bash
shuf -i 1-10
```

Isso irá gerar uma sequência de números aleatórios entre 1 e 10. Você também pode especificar um intervalo, como "1-100" ou "10-50". Além disso, você pode adicionar a opção "-r" para permitir repetições na sequência de números.

# Veja também
- [Bash Guide for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Random number generation in Bash](https://www.howtogeek.com/399958/how-to-generate-random-numbers-in-bash/)
- [Random.org - Ferramenta de geração de números aleatórios](https://www.random.org/)