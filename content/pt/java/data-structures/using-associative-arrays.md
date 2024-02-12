---
title:                "Usando arrays associativos"
aliases:
- /pt/java/using-associative-arrays/
date:                  2024-01-30T19:11:42.308637-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando arrays associativos"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê e Porquê?

Em Java, arrays associativos, ou mapas, permitem armazenar pares chave-valor para uma busca e manipulação de dados eficiente. Programadores os utilizam para tarefas como contar ocorrências de itens ou mapear usuários às suas permissões porque eles oferecem acesso e atualizações rápidas.

## Como Fazer:

Java não possui arrays associativos embutidos como algumas linguagens possuem, mas fornece a interface `Map` e classes como `HashMap` e `TreeMap` para preencher esse papel. Veja como usar um `HashMap`:

```Java
import java.util.HashMap;
import java.util.Map;

public class AprenderMapas {
    public static void main(String[] args) {
        // Criando um HashMap
        Map<String, Integer> idadeDosAmigos = new HashMap<>();
        
        // Adicionando elementos
        idadeDosAmigos.put("Alice", 24);
        idadeDosAmigos.put("Bob", 30);
        idadeDosAmigos.put("Charlie", 28);

        // Acessando elementos
        System.out.println("Idade da Alice: " + idadeDosAmigos.get("Alice"));
        
        // Lidando com chaves inexistentes
        System.out.println("Idade de alguém que não está no mapa: " + idadeDosAmigos.getOrDefault("Dan", -1));

        // Iterando sobre os elementos
        for (Map.Entry<String, Integer> entrada : idadeDosAmigos.entrySet()) {
            System.out.println(entrada.getKey() + " tem " + entrada.getValue() + " anos de idade.");
        }
    }
}
```

Saída de Exemplo:

```
Idade da Alice: 24
Idade de alguém que não está no mapa: -1
Alice tem 24 anos de idade.
Bob tem 30 anos de idade.
Charlie tem 28 anos de idade.
```

`HashMap` é apenas uma implementação. Se suas chaves são únicas e você precisa delas ordenadas, considere `TreeMap`. Para um mapa que retém a ordem de inserção, `LinkedHashMap` é o seu aliado.

## Mergulho Profundo

Mapas em Java fazem parte do Framework de Coleções, introduzido no JDK 1.2, mas viram melhorias significativas ao longo dos anos, incluindo a introdução do método `forEach` no Java 8 para uma iteração mais fácil sobre as entradas. A escolha da implementação do mapa (`HashMap`, `LinkedHashMap`, `TreeMap`) deve ser ditada pelas suas necessidades específicas em termos de ordenação e desempenho. Por exemplo, `HashMap` oferece desempenho de tempo O(1) para as operações básicas (obter e colocar), assumindo que a função hash dispersa os elementos adequadamente entre os baldes. No entanto, se você precisa de ordenação baseada na ordenação natural ou comparadores personalizados, `TreeMap` é a escolha certa, fornecendo tempo O(log n) para inserção e busca.

Antes da introdução do `Map`, arrays associativos geralmente eram implementados com dois arrays paralelos (um para chaves, outro para valores) ou estruturas de dados personalizadas com menos eficiência. Alternativas atuais para `Map` e suas implementações poderiam incluir bibliotecas de terceiros oferecendo mapas especializados, como mapas bidirecionais (BiMap na biblioteca Guava do Google) para casos onde você precisa encontrar uma chave pelo seu valor de forma eficiente. Contudo, para a maioria dos casos de uso em Java, os mapas da biblioteca padrão são robustos e flexíveis o suficiente para lidar com a tarefa.
