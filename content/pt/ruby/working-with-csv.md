---
title:                "Trabalhando com CSV"
date:                  2024-01-19
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"

category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Trabalhar com CSV no Ruby é lidar com dados em formato de texto separados por vírgulas, algo comum para importar e exportar dados de planilhas e bancos de dados. Fazemos isso porque é um jeito prático e universal de manipular dados simples, que até programas não especializados conseguem entender.

## Como Fazer:
```ruby
require 'csv'

# Lendo de um arquivo CSV
CSV.foreach("exemplo.csv", headers: true) do |row|
  puts row["Nome"] + " - " + row["Profissão"]
end

# Produzindo um arquivo CSV
CSV.open("saida.csv", "wb") do |csv|
  csv << ["Nome", "Idade"]
  csv << ["Marcela", 30]
  csv << ["João", 22]
end
```

Saída de exemplo ao ler um arquivo CSV:
```
Marcela - Engenheira de Software
João - Analista de Dados
```

## Mergulho Profundo:
CSV (Comma-Separated Values ou Valores Separados por Vírgula) é um formato de arquivo que existe desde antes dos anos 70. Alternativas incluem JSON e XML, que são mais complexas mas também mais poderosas. O Ruby fornece a biblioteca `csv` padrão, que faz o parse e a geração de arquivos CSV de forma intuitiva e simples.

## Veja Também:
- [Documentação oficial da classe CSV do Ruby](https://ruby-doc.org/stdlib-3.1.0/libdoc/csv/rdoc/CSV.html)
- [Ruby-Doc.org para aprender mais sobre métodos da classe CSV](https://www.ruby-doc.org/)
- [Artigo sobre CSV na Wikipedia](https://pt.wikipedia.org/wiki/Comma-separated_values)
