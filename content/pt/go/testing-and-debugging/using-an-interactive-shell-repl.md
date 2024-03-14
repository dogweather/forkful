---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:21.903918-07:00
description: "Um shell interativo, ou Loop de Leitura-Avalia\xE7\xE3o-Impress\xE3\
  o (REPL), permite que voc\xEA experimente c\xF3digo Go em tempo real, executando\
  \ comandos e obtendo\u2026"
lastmod: '2024-03-13T22:44:46.062473-06:00'
model: gpt-4-0125-preview
summary: "Um shell interativo, ou Loop de Leitura-Avalia\xE7\xE3o-Impress\xE3o (REPL),\
  \ permite que voc\xEA experimente c\xF3digo Go em tempo real, executando comandos\
  \ e obtendo\u2026"
title: Usando um shell interativo (REPL)
---

{{< edit_this_page >}}

## O Que & Por Que?

Um shell interativo, ou Loop de Leitura-Avaliação-Impressão (REPL), permite que você experimente código Go em tempo real, executando comandos e obtendo feedback imediato. Essa abordagem é amplamente utilizada para aprendizado, depuração e criação de protótipos, pois ela elimina o ciclo tradicional de editar-compilar-executar, tornando o processo de desenvolvimento mais rápido e intuitivo.

## Como Fazer:

Embora o Go não inclua um REPL embutido, a comunidade criou ferramentas como o `gore` para preencher essa lacuna. Primeiro, instale o `gore` executando:

```
$ go get -u github.com/motemen/gore
```

Uma vez instalado, inicie o `gore` digitando `gore` no seu terminal:

```
$ gore
```

Você deverá ver um prompt pronto para aceitar comandos Go. Vamos tentar um exemplo simples:

```
gore> :import fmt
gore> fmt.Println("Olá, REPL Go!")
```

Você verá uma saída como:

```
Olá, REPL Go!
```

Variáveis e definições de funções funcionam como esperado. Você pode declarar uma função:

```
gore> :import math
gore> areaCircle := func(raio float64) float64 {
...> return math.Pi * raio * raio
...> }
gore> fmt.Println("Área do círculo com raio 4:", areaCircle(4))
```

E obter a saída imediatamente:

```
Área do círculo com raio 4: 50.26548245743669
```

## Aprofundamento:

O conceito de um REPL é antigo, remontando às máquinas Lisp dos anos 1960, proporcionando uma experiência de programação interativa. Ao contrário de linguagens como Python ou JavaScript, o Go foi projetado sem um REPL, focando em vez disso em binários compilados para desempenho e simplicidade. Isso reflete a filosofia de simplicidade do Go e seu design para software escalável e mantível.

No entanto, ferramentas como `gore` ou `goplay` mostram a engenhosidade da comunidade Go em preencher essa lacuna. Essas ferramentas analisam dinamicamente o código Go e usam o pacote `go/eval` ou mecanismos similares para executá-lo em tempo real, embora com algumas limitações em comparação a um ambiente REPL nativo. Essas limitações decorrem do sistema de tipos e modelo de compilação do Go, que podem tornar a avaliação em tempo real desafiadora.

Embora os ambientes REPL sejam excepcionalmente úteis para educação e testes rápidos, o ecossistema Go geralmente se inclina para processos tradicionais de compilação e execução para a maioria das tarefas de desenvolvimento. IDEs e editores com suporte ao Go, como o Visual Studio Code ou o GoLand, oferecem ferramentas integradas para testes e depuração que aliviam muito a necessidade de um REPL para desenvolvimento profissional.

Para programação exploratória, prototipagem, ou aprendizado, no entanto, REPLs como o `gore` oferecem uma alternativa valiosa, permitindo que programadores acostumados com REPLs em outras linguagens desfrutem de uma experiência similar em Go.
