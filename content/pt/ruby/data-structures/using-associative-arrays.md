---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:51.598935-07:00
description: "Como fazer: Criar e usar hashes no Ruby \xE9 simples. Voc\xEA pode inicializar\
  \ um hash vazio, preench\xEA-lo com pares chave-valor, acessar valores por suas\
  \ chaves\u2026"
lastmod: '2024-03-13T22:44:47.086648-06:00'
model: gpt-4-0125-preview
summary: "Criar e usar hashes no Ruby \xE9 simples."
title: Usando arrays associativos
weight: 15
---

## Como fazer:
Criar e usar hashes no Ruby é simples. Você pode inicializar um hash vazio, preenchê-lo com pares chave-valor, acessar valores por suas chaves e mais. Veja como fazer:

```Ruby
# Criando um hash
my_hash = { "name" => "John Doe", "age" => 30 }

# Outra maneira de criar um hash
another_hash = Hash.new
another_hash["position"] = "Desenvolvedor"

# Acessando valores do hash
puts my_hash["name"] # Saída: John Doe

# Adicionando um novo par chave-valor
my_hash["language"] = "Ruby"
puts my_hash # Saída: {"name"=>"John Doe", "age"=>30, "language"=>"Ruby"}

# Iterando através de um hash
my_hash.each do |key, value|
  puts "#{key}: #{value}"
end
# Saída:
# name: John Doe
# age: 30
# language: Ruby
```

Você também pode usar símbolos como chaves mais eficientes:

```Ruby
# Usando símbolos para chaves
symbol_hash = { name: "Jane Doe", age: 22 }
puts symbol_hash[:name] # Saída: Jane Doe
```

## Mergulho Profundo:
O conceito de arrays associativos não é único do Ruby; muitas linguagens os implementam sob diversos nomes, como dicionários no Python ou objetos no JavaScript (quando usados como pares chave-valor). Nos estágios iniciais do Ruby, os hashes eram um pouco mais lentos e não tão versáteis. No entanto, com o tempo, a implementação de hashes no Ruby tornou-se altamente otimizada, especialmente para chaves de símbolos, tornando-os extremamente eficientes para acessos e atualizações frequentes.

Os hashes do Ruby se destacam pela sua facilidade de uso sintático e flexibilidade - você pode usar quase qualquer tipo de objeto como chave, embora símbolos e strings sejam os mais comuns. Internamente, os hashes do Ruby são implementados usando um algoritmo de hashing que equilibra velocidade e eficiência de memória, mesmo quando o número de elementos aumenta.

Embora os hashes sejam incrivelmente versáteis, eles não são a solução final para armazenamento de dados no Ruby. Para coleções ordenadas, arrays são mais apropriados, e para conjuntos de itens únicos, um Set pode ser uma escolha melhor. Além disso, para estruturas de dados muito complexas, criar classes personalizadas pode ser aconselhável.

Lembre-se, a escolha de usar um hash versus outras estruturas de dados depende em grande parte do caso de uso específico - hashes são excelentes para pesquisas rápidas e manutenção de associações entre chaves únicas e seus valores.
