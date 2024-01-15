---
title:                "Escrevendo um arquivo de texto"
html_title:           "Ruby: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Por Que

Escrever arquivos de texto é uma tarefa fundamental na programação Ruby, pois permite armazenar e acessar informações importantes de forma organizada e fácil de usar.

# Como Fazer

Para escrever um arquivo de texto usando Ruby, primeiro precisamos abrir um novo arquivo utilizando o método `File.open`. Em seguida, utilizamos o método `write` para inserir o conteúdo que desejamos no arquivo. Por exemplo:

```Ruby
File.open("meu_arquivo.txt", "w") do |file|
  file.write("Olá, mundo!")
end
```

Isso criará um arquivo chamado "meu_arquivo.txt" e inserirá a frase "Olá, mundo!" dentro dele. Podemos usar o método `puts` para adicionar novas linhas ao arquivo:

```Ruby
File.open("meu_arquivo.txt", "a") do |file|
  file.puts("Eu sou um desenvolvedor Ruby!")
end
```

Além disso, é importante sempre fechar o arquivo após terminarmos de escrever nele, utilizando o método `close`:

```Ruby
File.open("meu_arquivo.txt", "a") do |file|
  file.puts("Isso é tudo, pessoal!")
end

file.close
```

Ao abrir o arquivo "meu_arquivo.txt", veremos que todas as linhas foram adicionadas com sucesso.

# Mergulho Profundo

Além dos métodos `write` e `puts`, existem outras maneiras de escrever em arquivos de texto em Ruby. Por exemplo, podemos usar o método `print`, que não adiciona uma nova linha após o conteúdo. Também podemos concatenar diferentes variáveis e strings em uma única linha:

```Ruby
nome = "João"
idade = 30

File.open("meu_arquivo.txt", "a") do |file|
  file.print("Olá, eu me chamo #{nome} e tenho #{idade} anos.")
end
```

Outra opção é utilizar o método `printf`, que formata a saída de acordo com as especificações dadas. Por exemplo:

```Ruby
preco = 99.99
File.open("meu_arquivo.txt", "a") do |file|
  file.printf("O preço do produto é R$%.2f.", preco)
end
```

Isso resultará em uma saída formatada como "O preço do produto é R$99.99.".

# Veja Também

Aqui estão alguns recursos adicionais para aprender mais sobre como escrever arquivos de texto em Ruby:

- [Documentação oficial do Ruby sobre o uso de arquivos](https://ruby-doc.org/core-2.7.2/File.html)
- [Tutorial sobre escrita em arquivos com Ruby na DevMedia](https://www.devmedia.com.br/escrita-de-arquivos-em-ruby/38327)
- [Vídeo tutorial sobre o uso de arquivos em Ruby no canal Código Fonte TV](https://www.youtube.com/watch?v=6XgkCkOhlTA)