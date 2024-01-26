---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:48:30.401486-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Gerar números aleatórios é simplesmente o ato de criar valores que não seguem um padrão previsível. Programadores os utilizam para tudo, desde jogos até simulações e segurança de dados.

## Como Fazer:
Vamos à prática. No Bash, você pode gerar números aleatórios com `$RANDOM` ou com a gama de comandos como `shuf` e `awk`. Aqui estão alguns exemplos:

```Bash
# Gerar um número aleatório com $RANDOM
echo $RANDOM

# Limitando o valor a um máximo de 100
echo $((RANDOM % 100))

# Número aleatório entre um intervalo (10-20)
echo $((RANDOM % (20 - 10 + 1) + 10))

# Usando shuf para gerar um número entre 1 e 10
shuf -i 1-10 -n 1

# Utilizando awk para um número de ponto flutuante entre 0 e 1
awk 'BEGIN{srand(); print rand()}'
```

Exemplo de saída para o comando `$RANDOM`:
```
$ echo $RANDOM
25703
```

## Mergulho Profundo:
O comando `$RANDOM` é um recurso interno do Bash, que gera números pseudo-aleatórios. Nada aqui é verdadeiramente aleatório; é tudo uma imitação baseada em algoritmos.

Historicamente, a necessidade de números aleatórios em programação é tão antiga quanto a própria programação. Alternativas ao `$RANDOM` do Bash podem incluir o uso de dispositivos de sistema (`/dev/random` e `/dev/urandom`) ou a implementação de geradores de números aleatórios específicos de um projeto, como o Mersenne Twister.

Quanto à implementação, `$RANDOM` gera números entre 0 e 32767. Para limitar ou expandir esse intervalo, usa-se módulo e adição. Mas cuidado: abusar do `$RANDOM` pode levar a padrões repetitivos - não é ideal para criptografia.

## Veja Também:
Para aprofundar:
- `man bash`: para mais informações sobre variáveis internas do Bash.
- `/dev/random` e `/dev/urandom`: para entender como o Linux lida com a aleatoriedade no nível do sistema.
- https://www.random.org/: para números verdadeiramente aleatórios, gerados por fenômenos atmosféricos.

Para aprender mais sobre segurança e números aleatórios:
- https://www.owasp.org/: Open Web Application Security Project, uma comunidade online que cria artigos, metodologias e ferramentas para segurança na web.
- NIST's Recommendations for Random Number Generation Using Deterministic Random Bit Generators: Uma leitura profunda sobre padrões de geração de número aleatório para criptografia.
