---
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:24.446785-07:00
description: "Arrays associativos, ou dicion\xE1rios, em Haskell s\xE3o todos sobre\
  \ mapear chaves a valores para uma busca r\xE1pida e um gerenciamento eficiente\
  \ de dados.\u2026"
lastmod: '2024-03-13T22:44:46.618423-06:00'
model: gpt-4-0125-preview
summary: "Arrays associativos, ou dicion\xE1rios, em Haskell s\xE3o todos sobre mapear\
  \ chaves a valores para uma busca r\xE1pida e um gerenciamento eficiente de dados.\u2026"
title: Usando arrays associativos
weight: 15
---

## O Quê & Por Quê?

Arrays associativos, ou dicionários, em Haskell são todos sobre mapear chaves a valores para uma busca rápida e um gerenciamento eficiente de dados. Programadores os utilizam para lidar com coleções de elementos emparelhados, onde buscar um elemento é fácil, comparado a listas.

## Como fazer:

Haskell não possui arrays associativos prontos da mesma forma que algumas outras linguagens, mas oferece uma biblioteca padrão poderosa chamada `Data.Map` para trabalhar com pares chave-valor. Vamos arregaçar as mangas e ver como usá-los!

Primeiro, certifique-se de importá-la:
```Haskell
import qualified Data.Map as Map
```

Criar um mapa é direto. Vamos criar um com algumas linguagens de programação e seus paradigmas:
```Haskell
let languages = Map.fromList [("Haskell", "Funcional"), ("Python", "Imperativo"), ("Prolog", "Lógico")]
```

Agora, que tal pegar o paradigma do Haskell?
```Haskell
Map.lookup "Haskell" languages
-- saída: Just "Funcional"
```

Adicionar uma nova linguagem é fácil:
```Haskell
let languagesUpdated = Map.insert "Rust" "Sistemas" languages
```

E se quisermos listar todas as linguagens? Use `Map.keys`:
```Haskell
Map.keys languagesUpdated
-- saída: ["Haskell","Python","Prolog","Rust"]
```

Para listar os paradigmas, use `Map.elems`:
```Haskell
Map.elems languagesUpdated
-- saída: ["Funcional","Imperativo","Lógico","Sistemas"]
```

Estas operações básicas devem cobrir a maioria dos usos, mas há muito mais para explorar em `Data.Map`!

## Aprofundando

O módulo `Data.Map` na biblioteca padrão de Haskell é construído em cima de árvores binárias balanceadas, especificamente árvores AVL. Essa escolha garante que a maioria das operações no mapa, como inserção, remoção, e busca, possam ser feitas em tempo O(log n), onde n é o número de elementos no mapa. É uma escolha eficiente para muitos casos de uso, embora não seja a mais rápida absoluta para todos os cenários.

Há uma nuance histórica também: antes do `Data.Map` se tornar a opção principal, programadores Haskell frequentemente usavam listas de pares para simular arrays associativos. Entretanto, operações nessas estruturas são O(n) para busca, tornando o `Data.Map` uma melhoria significativa em termos de desempenho.

Agora, apesar da eficiência e utilidade do `Data.Map`, nem sempre é a melhor ferramenta para cada trabalho. Para tarefas altamente sensíveis ao desempenho, onde até tempos de busca O(log n) são lentos demais, ou onde as chaves são sempre valores inteiros, arrays ou tabelas de hash (via `Data.HashMap`) podem oferecer um melhor desempenho com tempos de acesso O(1).

O ecossistema Haskell permite uma variedade de estruturas de dados para atender diferentes necessidades, e `Data.Map` é uma excelente escolha de uso geral para arrays associativos, equilibrando facilidade de uso, flexibilidade e desempenho.
