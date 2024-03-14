---
date: 2024-01-26 03:43:28.162262-07:00
description: "Arredondar n\xFAmeros \xE9 ajustar um n\xFAmero para o inteiro mais\
  \ pr\xF3ximo, ou para uma certa precis\xE3o decimal. Arredondamos n\xFAmeros para\
  \ simplific\xE1-los para\u2026"
lastmod: '2024-03-13T22:44:46.191814-06:00'
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros \xE9 ajustar um n\xFAmero para o inteiro mais pr\xF3\
  ximo, ou para uma certa precis\xE3o decimal. Arredondamos n\xFAmeros para simplific\xE1\
  -los para\u2026"
title: "Arredondamento de n\xFAmeros"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Arredondar números é ajustar um número para o inteiro mais próximo, ou para uma certa precisão decimal. Arredondamos números para simplificá-los para leitura humana, reduzir a carga computacional, ou satisfazer requisitos numéricos específicos.

## Como fazer:
Em Clojure, usamos principalmente `Math/round`, `Math/floor` e `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

Para casas decimais específicas, multiplicamos, arredondamos e dividimos:

```clojure
(let [num 3.14159
      escala 1000]
  (/ (Math/round (* num escala)) escala)) ; => 3.142
```

## Aprofundamento
Antes dos sofisticados linguagens de programação, o arredondamento era um processo manual, imagine o ábaco ou papel. Na programação, é crucial para a representação de números devido às limitações de precisão de ponto flutuante.

Alternativas para arredondamento incluem o uso da classe `BigDecimal` para controle de precisão ou bibliotecas como `clojure.math.numeric-tower` para funções matemáticas avançadas. O `Math/round` de Clojure depende das funções `Math.round`, `Math/floor` e `Math/ceil` do Java, o que significa que herda as mesmas nuances de float e double.

Do ponto de vista da implementação, ao arredondar em Clojure, lembre-se de que ele automaticamente usa precisão dupla ao lidar com decimais. Cuidado com erros de arredondamento!

## Veja Também
- API de Matemática do Clojure: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- API de Matemática do Java: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Entendendo a Precisão de Ponto Flutuante: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
