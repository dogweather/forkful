---
date: 2024-01-27 20:32:34.567643-07:00
description: "Como fazer: No Bash, a vari\xE1vel `$RANDOM` \xE9 a melhor op\xE7\xE3\
  o para gerar n\xFAmeros aleat\xF3rios. Toda vez que voc\xEA a referenciar, o Bash\
  \ fornece um inteiro\u2026"
lastmod: '2024-03-13T22:44:46.746647-06:00'
model: gpt-4-0125-preview
summary: "No Bash, a vari\xE1vel `$RANDOM` \xE9 a melhor op\xE7\xE3o para gerar n\xFA\
  meros aleat\xF3rios."
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
weight: 12
---

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
