---
title:                "Gerando números aleatórios."
html_title:           "C#: Gerando números aleatórios."
simple_title:         "Gerando números aleatórios."
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Gerar números aleatórios é um processo comumente usado na programação para obter valores aleatórios em um determinado intervalo. Isso é particularmente útil para jogos, sorteios ou qualquer situação em que a aleatoriedade é necessária. Os programadores usam esse recurso para adicionar variedade e imprevisibilidade aos seus programas.

## Como fazer:

Gerar números aleatórios é bastante simples em C#. Basta utilizar a classe Random, que já está presente na biblioteca padrão do C#, e chamar o método Next() para obter um número aleatório. Por exemplo:

```C#
Random rand = new Random(); //cria uma instância da classe Random
int numero = rand.Next(1, 100); //gera um número inteiro entre 1 e 100
Console.WriteLine(numero); //imprime o número gerado
```

A saída deste código pode ser um número qualquer entre 1 e 100, o que torna o processo imprevisível e aleatório.

## Fundamentação:

A geração de números aleatórios tem sido usada na computação desde os primórdios, com o objetivo de simular o comportamento caótico da natureza. Antes do surgimento dos computadores, os números aleatórios eram obtidos através de métodos físicos, como dados, moedas e roletas. Com o avanço da tecnologia, tornou-se possível gerar números aleatórios de forma mais eficiente e precisa através de algoritmos.

Existem também outras maneiras de gerar números aleatórios em C#, como por exemplo, utilizando a classe RNGCryptoServiceProvider, que é mais indicada para fins de segurança e criptografia. No entanto, para a maioria dos casos, o uso da classe Random é o suficiente.

## Ver também:

Saiba mais sobre a classe Random na documentação oficial da Microsoft: https://docs.microsoft.com/pt-br/dotnet/api/system.random?view=netcore-3.1

Para uma análise mais aprofundada sobre a geração de números aleatórios em computadores, confira este artigo da Wikipedia (em inglês): https://en.wikipedia.org/wiki/Random_number_generation