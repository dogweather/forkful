---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Criando um arquivo temporário em Ruby

## O Que & Por Quê?
Criar um arquivo temporário é uma prática comum em programação. Ela proporciona um espaço transitório de armazenamento de dados, geralmente necessário para operações de processamento de arquivos massivos ou transferências de dados.

## Como Fazer:
Vamos analisar como criar um arquivo temporário em Ruby. O módulo 'Tempfile' do Ruby facilita isso para nós. Aqui está um exemplo de uso:

```Ruby
require 'tempfile'

temp = Tempfile.new('meu_arquivo_temp.txt')
temp << 'Algum texto aqui'
temp.close

open(temp.path) do |f|
  puts f.read
end
```
O output será: 'Algum texto aqui'

## Imersão Profunda

### Contexto Histórico
Historicamente, os arquivos temporários eram criados manualmente, o que acarretava diversos problemas, como a sobrecarga de limpeza e a gestão de nomes de arquivos únicos. A introdução da classe `Tempfile` em Ruby simplificou esses aspectos.

### Alternativas
Existe uma alternativa ao módulo `Tempfile`. O módulo `IO` possui um método `IO.pipe` que cria um par de pipe sem nome e a memória é automaticamente limpa após o uso.

```Ruby
require 'io'

r, w = IO.pipe
w << "Olá, mundo!"
w.close
puts r.read
```
Resultado: 'Olá, mundo!'

### Detalhes de Implementação
'A classe Tempfile implementa um objeto de arquivo temporário com um nome único no sistema de arquivos. Os arquivos temporários são automaticamente removidos quando o objeto é coletado pelo Garbage Collector.

## Veja Também
- Documentação oficial do Ruby sobre Tempfile: https://docs.ruby-lang.org/en/3.0.0/Tempfile.html 
- Documentação oficial do Ruby sobre o IO.pipe: https://docs.ruby-lang.org/en/3.0.0/IO.html
- StackOverflow com discussões úteis sobre arquivos temporários: https://stackoverflow.com/questions/tagged/tempfile