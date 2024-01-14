---
title:                "Elm: Gerando números aleatórios."
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em Elm?

Gerar números aleatórios é uma habilidade útil em qualquer linguagem de programação, incluindo Elm. Essa funcionalidade é especialmente útil quando se deseja criar jogos, simulações ou testes de desempenho. Portanto, se você está trabalhando em um desses tipos de projetos, aprender a gerar números aleatórios em Elm pode ser muito útil.

## Como fazer isso em Elm?

Em Elm, gerar números aleatórios é muito simples. Tudo o que você precisa fazer é usar a função `Random` e definir o tipo de dado que você deseja gerar. Por exemplo, se você quiser gerar um número inteiro aleatório entre 1 e 100, você pode usar o seguinte código:

```Elm
import Random

myRandomNumber : Int
myRandomNumber =
  Random.int 1 100
```

Isso irá gerar aleatoriamente um número inteiro entre 1 e 100 e atribuí-lo à variável `myRandomNumber`. Você também pode gerar números decimais, caracteres ou até mesmo listas de números aleatórios em Elm. Basta consultar a documentação para ver todas as possibilidades.

## Aprofundando nas gerações de números aleatórios em Elm

Agora que você sabe como gerar números aleatórios em Elm, é importante entender como essa funcionalidade funciona em mais detalhes. Em Elm, a geração de números aleatórios é baseada em *sementes* e *geradores*. Uma semente é um valor inicial que é usado para gerar os números aleatórios e um gerador é uma função que recebe uma semente e retorna um valor aleatório. Quando você usa a função `Random`, ela cria automaticamente uma semente aleatória e passa essa semente para o gerador que você especificou. Isso garante que cada vez que você executar o programa, um número aleatório diferente será gerado.

## Veja também

- Documentação oficial sobre geração de números aleatórios em Elm: https://guide.elm-lang.org/effects/random.html
- Exemplo de aplicação de geração de números aleatórios em Elm: https://github.com/118101267/elm-random-example
- Tutorial em vídeo sobre como gerar números aleatórios em Elm: https://www.youtube.com/watch?v=F6XuURwVuTk