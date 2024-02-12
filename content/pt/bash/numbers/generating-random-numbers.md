---
title:                "Geração de números aleatórios"
aliases: - /pt/bash/generating-random-numbers.md
date:                  2024-01-27T20:32:34.567643-07:00
model:                 gpt-4-0125-preview
simple_title:         "Geração de números aleatórios"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Gerar números aleatórios no Bash oferece uma maneira de introduzir imprevisibilidade nos scripts, o que é essencial para tarefas como gerar senhas seguras, simular dados ou para programar jogos. Os programadores aproveitam essa capacidade para adicionar variabilidade aos seus scripts ou para testar seus programas sob uma variedade de condições geradas aleatoriamente.

## Como fazer:
No Bash, a variável `$RANDOM` é a melhor opção para gerar números aleatórios. Toda vez que você a referenciar, o Bash fornece um inteiro pseudoaleatório entre 0 e 32767. Vamos explorar alguns exemplos práticos:

```Bash
# Uso básico de $RANDOM
echo $RANDOM

# Gerando um número aleatório em um intervalo especificado (0-99 aqui)
echo $(( RANDOM % 100 ))

# Gerando um número aleatório mais "seguro", adequado para senhas ou chaves
# Usando /dev/urandom com o comando od
head -c 8 /dev/urandom | od -An -tu4

# Definindo uma semente para RANDOM para reprodutibilidade
RANDOM=42; echo $RANDOM
```

Exemplo de saída (nota: a saída real variará, já que os números são aleatórios):
```Bash
16253
83
3581760565
17220
```

## Aprofundamento
O mecanismo por trás do `$RANDOM` do Bash gera números pseudoaleatórios, o que significa que eles seguem um algoritmo e podem, em teoria, ser previsíveis - uma possível falha de segurança para aplicações que requerem genuína imprevisibilidade. Aplicações criptográficas modernas geralmente requerem aleatoriedade derivada de fenômenos físicos ou de hardware projetado especificamente para gerar dados aleatórios, como `/dev/urandom` ou `/dev/random` no Linux, que coletam ruído ambiental.

Para tarefas casuais ou não críticas à segurança, `$RANDOM` é suficiente e oferece o benefício da simplicidade. No entanto, para fins criptográficos ou onde a qualidade da aleatoriedade é crítica, os desenvolvedores devem procurar outras ferramentas e linguagens projetadas com a criptografia em mente, como OpenSSL ou linguagens de programação com bibliotecas robustas de gerador de números aleatórios.

Enquanto o `$RANDOM` do Bash serve ao seu propósito em scripts que requerem números aleatórios básicos, suas limitações devem direcionar os desenvolvedores para soluções mais robustas para aplicações onde a qualidade ou segurança da aleatoriedade importa.
