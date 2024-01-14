---
title:    "Fish Shell: Gerando números aleatórios"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que
Gerar números aleatórios é uma habilidade essencial para tarefas como criar senhas seguras, simular cenários e realizar testes de software. Aprender a fazer isso usando o Fish Shell pode ser útil para programadores e entusiastas de tecnologia.

## Como fazer
Gerar números aleatórios no Fish Shell é muito simples. Use o comando “rand”, seguido pelo número máximo que você deseja que seu número aleatório tenha.

```
Fish Shell rand 100
```

Este comando irá gerar um número aleatório entre 0 e 100. Você também pode usar as opções “-l” e “-u” para especificar o limite inferior e superior do número aleatório.

```
Fish Shell rand -l 50 -u 100
```

Este comando irá gerar um número aleatório entre 50 e 100. Você pode ainda usar a opção “-n” para gerar mais de um número aleatório de uma vez.

```
Fish Shell rand -n 5 50
```

Este comando irá gerar cinco números aleatórios entre 0 e 50.

## Aprofundando
O Fish Shell usa o algoritmo de gerador de números pseudoaleatórios (PRNG) Mersenne Twister para gerar seus números aleatórios. Isso significa que, embora os números possam parecer aleatórios, eles são gerados de uma sequência previsível. Portanto, não é recomendado utilizar esses números para fins de segurança, como gerar senhas.

Você também pode definir uma semente para o seu gerador de números aleatórios, usando a opção “-s”. Isso permite que você sempre gere a mesma sequência de números aleatórios, o que pode ser útil para fins de replicação em testes ou simulações.

## Veja também
- [Fish Shell documentação - rand] (https://fishshell.com/docs/current/cmds/rand.html)
- [Artigo Wikipedia sobre gerador Mersenne Twister] (https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Tutorial sobre como gerar números aleatórios seguros em Fish Shell] (https://www.digitalocean.com/community/tutorials/how-to-generate-secure-random-numbers-in-fish-shell)