---
title:                "Gerando números aleatórios"
html_title:           "Gleam: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

O que e por que gerar numeros aleatorios?

Gerar numeros aleatorios e um recurso importante para programadores, pois permite criar um elemento de aleatoriedade em seus codigos. Isso pode ser util em jogos, sorteios, e ate mesmo na criacao de senhas seguras. 

Como fazer: 

Para gerar numeros aleatorios em Gleam, voce pode usar a funcao ```Gleam.Math.random()```. Esta funcao aceita um parametro opcional, o qual define o limite maximo do numero aleatorio gerado. Abaixo esta um exemplo de como usar essa funcao para gerar um numero entre 1 e 100:

```
let numero = Gleam.Math.random(100)
```

Voce pode entao imprimir o numero gerado usando a funcao ```Gleam.IO.inspect()```. O codigo completo fica assim:

```
let numero = Gleam.Math.random(100)
Gleam.IO.inspect(numero)
```

Isso ira produzir um resultado semelhante a este: 

```
23
```

Profundidade:

A geracao de numeros aleatorios tem sido uma area de estudo na computacao ha muitos anos. Existem diversas maneiras de se gerar numeros aleatorios, que vao desde algoritmos matematicos ate hardware especializado. Em Gleam, a funcao ```Gleam.Math.random()``` utiliza um algoritmo de geracao de numeros pseudoaleatorios para produzir resultados que possuem uma aparencia de aleatoriedade.

Veja tambem:

Voce pode aprender mais sobre a geracao de numeros aleatorios e como utiliza-la em seus codigos, visitando os seguintes recursos:

- [Documentacao oficial do Gleam](https://gleam.run) 
- [Artigo sobre geracao de numeros pseudoaleatorios](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)