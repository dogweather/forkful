---
title:                "Usando arrays associativos"
aliases: - /pt/powershell/using-associative-arrays.md
date:                  2024-01-30T19:12:31.355006-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando arrays associativos"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Arrays associativos, também conhecidos como tabelas hash ou dicionários no PowerShell, permitem armazenar dados em pares chave-valor, tornando a recuperação de dados direta e eficiente. Os programadores os utilizam para armazenar dados relacionados de maneira que é fácil acessar por chave.

## Como Fazer:

Criar e usar arrays associativos no PowerShell é bastante direto. Veja como fazer a mágica:

**Criando um array associativo:**

```PowerShell
$meuArrayAssociativo = @{}
$meuArrayAssociativo["nome"] = "Alex"
$meuArrayAssociativo["idade"] = 25
$meuArrayAssociativo["trabalho"] = "Engenheiro"
```

Este trecho de código cria um array associativo com três pares chave-valor.

**Acessando valores:**

Para obter um valor, referencie sua chave:

```PowerShell
Write-Output $meuArrayAssociativo["nome"]
```

**Saída de Amostra:**

```
Alex
```

**Adicionando ou modificando dados:**

Basta usar a chave para adicionar um novo par ou modificar um existente:

```PowerShell
$meuArrayAssociativo["localização"] = "Nova York" # Adiciona um novo par chave-valor
$meuArrayAssociativo["trabalho"] = "Engenheiro Sênior" # Modifica um par existente
```

**Iterando sobre um array associativo:**

Itere pelas chaves e valores assim:

```PowerShell
foreach ($chave in $meuArrayAssociativo.Keys) {
  $valor = $meuArrayAssociativo[$chave]
  Write-Output "$chave : $valor"
}
```

**Saída de Amostra:**

```
nome : Alex
idade : 25
trabalho : Engenheiro Sênior
localização : Nova York
```

## Aprofundando

O conceito de arrays associativos é comum em muitas linguagens de programação, normalmente chamado de dicionário, mapa ou tabela hash, dependendo da linguagem. No PowerShell, os arrays associativos são implementados como tabelas hash, que são bastante eficientes para procurar chaves, armazenar dados e manter uma coleção de chaves únicas.

Historicamente, os arrays associativos fornecem uma maneira de gerenciar coleções de objetos em que cada item pode ser rapidamente recuperado sem iterar pela coleção inteira, usando sua chave. A eficiência da recuperação e modificação de dados em arrays associativos os torna uma escolha preferida para várias tarefas. No entanto, eles têm limitações, como manter a ordem, para as quais dicionários ordenados ou objetos personalizados poderiam ser uma alternativa melhor.

Apesar de suas limitações, os arrays associativos/tabelas hash no PowerShell são incrivelmente flexíveis e uma ferramenta poderosa para scripts. Eles permitem o armazenamento dinâmico de dados e são particularmente úteis em configurações, manipulação de dados e em qualquer lugar que um formato de dados estruturado seja necessário sem a sobrecarga de uma definição de classe formal. Lembre-se, enquanto os arrays associativos são perfeitos para recuperação baseada em chave, se sua tarefa envolve estruturas de dados complexas ou requer manter uma ordem específica, você pode querer explorar outros tipos de dados ou objetos personalizados dentro do PowerShell.
