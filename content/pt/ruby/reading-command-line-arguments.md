---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

A leitura de argumentos da linha de comando permite que um programador passe informações para um programa no momento da execução. Ajuda a tornar um programa mais adaptável e a reutilizar métodos sem modificá-los.

## Como fazer:

Aqui está um exemplo simples de como ler os argumentos da linha de comando em Ruby:

```Ruby
# arquivo: args.rb
ARGV.each do |argumento|
  puts "Argumento passado: #{argumento}"
end
```

Você pode executar o programa acima para ver a saída:

```shell
$ ruby args.rb olá mundo
Argumento passado: olá
Argumento passado: mundo
```

## Aprofundando no Assunto

(1) Dentro do contexto histórico, a prática de passar argumentos através da linha de comando remonta aos primeiros dias de programação. Isso se mostrou extremamente útil ao longo do tempo, especificamente em tarefas de script e automação.

(2) Como alternativa, você poderia usar entradas padrão (STDIN), arquivos de configuração ou até mesmo interações com o usuário para conseguir entradas, mas os argumentos da linha de comando são geralmente a solução mais simples e direta.

(3) Quanto à implementação, em Ruby, os argumentos da linha de comando são acessados através da constante `ARGV`, que é um array contendo os valores passados na linha de comando.

## Veja Também

Se você deseja aprender mais sobre a linha de comando e os argumentos, aqui estão alguns links úteis:

- [Documentação Oficial do Ruby](https://docs.ruby-lang.org/en/#{ruby-version}/OptionParser.html)
- [Stack Overflow: Como funcionam os argumentos da linha de comando?](https://stackoverflow.com/questions/483952/how-do-command-line-arguments-work)
- [Tutorial completo sobre argumentos da linha de comando em Ruby](https://www.codecademy.com/articles/ruby-command-line-argv).