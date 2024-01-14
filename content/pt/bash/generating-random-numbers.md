---
title:                "Bash: Gerando números aleatórios"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

Gerar números aleatórios é uma habilidade importante em programação, especialmente em linguagens de script como Bash. Isso permite que você crie lógica de decisão, simule cenários e teste algoritmos. Além disso, a capacidade de gerar números aleatórios também pode ser útil em jogos e outras aplicações divertidas. 

## Como fazer isso em Bash?

Em Bash, você pode gerar números aleatórios usando o comando `shuf`. Este comando é usado para embaralhar linhas em um arquivo de entrada, mas também pode ser usado para gerar números aleatórios. Veja um exemplo simples abaixo:

```Bash
# Gerar um número aleatório entre 1 e 10
echo $((1 + $RANDOM % 10))
```

Este comando primeiro usa a variável interna `RANDOM` do Bash, que gera um número aleatório entre 0 e 32.767. Então, usamos a expressão aritmética `$(( ... ))` para adicionar 1 a esse número e obter um valor entre 1 e 32.767. Finalmente, usamos o operador de módulo (`%`) para limitar esse valor entre 0 e 9, e então adicionamos 1 novamente para obter um número entre 1 e 10.

Você também pode gerar vários números aleatórios em um loop, basta usar uma variável de iteração como `i` e um número como `10` no exemplo abaixo:

```Bash
# Gerar 10 números aleatórios entre 1 e 100
for i in {1..10}; do
  echo $((1 + $RANDOM % 100))
done
```

O comando `shuf` também possui opções adicionais que permitem gerar números aleatórios em uma faixa específica e até determinar a quantidade de caracteres que o número deve ter. Consulte o manual do `shuf` para obter mais informações sobre como usá-lo para gerar números aleatórios.

## Mergulho profundo

Gerar números aleatórios pode parecer simples, mas há algumas coisas a serem consideradas quando se trata de aleatoriedade. Por exemplo, os números gerados pelo `RANDOM` do Bash não são realmente aleatórios, mas sim pseudorrandômicos. Isso significa que eles são gerados por um algoritmo e podem ser repetidos se o mesmo algoritmo for usado. 

Se você precisa de números verdadeiramente aleatórios, pode considerar a instalação de um gerador de números aleatórios como `/dev/random` no seu sistema operacional. Este dispositivo de caracteres permite que você leia números aleatórios de uma fonte confiável, como movimentos atômicos ou ruído de rede. No entanto, isso pode depender do seu sistema operacional e do hardware do seu computador.

## Veja também

- Documentação oficial do `shuf`: https://linux.die.net/man/1/shuf
- Explicação sobre geradores de números aleatórios em Bash: https://www.geeksforgeeks.org/random-number-generator-in-bash/
- Como usar o dispositivo `/dev/random` em sistemas Linux: https://www.kernel.org/doc/Documentation/usb/random.txt