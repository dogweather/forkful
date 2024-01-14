---
title:    "Ruby: Escrevendo um arquivo de texto"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que

Escrever um arquivo de texto é uma habilidade fundamental para programadores de Ruby. Com essa habilidade, você poderá armazenar dados, ou até mesmo criar arquivos de configuração para seus programas. Além disso, escrever arquivos de texto é uma parte essencial do processo de aprendizado da linguagem Ruby.

## Como fazer

Para escrever um arquivo de texto em Ruby, você precisará seguir alguns passos simples:

1. Primeiro, abra o arquivo usando o método `File.new` e passando o nome do arquivo como um argumento. Por exemplo: `file = File.new("arquivo.txt", "w")`.
2. Em seguida, use o método `File.write` para inserir o conteúdo no arquivo. Por exemplo: `File.write(file, "Olá, mundo!")`, onde `file` é o nome da variável que representa o arquivo aberto anteriormente.
3. Por fim, feche o arquivo usando `file.close`.

Aqui está um exemplo completo de como criar um arquivo de texto simples no Ruby:

```
file = File.new("arquivo.txt", "w")
File.write(file, "Olá, mundo!")
file.close
```

Isso irá criar um arquivo chamado "arquivo.txt" no diretório do seu programa, com o conteúdo "Olá, mundo!".

## Mergulho profundo

Ao escrever um arquivo de texto em Ruby, é importante entender os modos de abertura de arquivo. O modo "w" usado no exemplo acima significa "escrita" e será usado se o arquivo não existir. No entanto, se o arquivo já existir, seu conteúdo será sobrescrito. Se você deseja adicionar conteúdo a um arquivo existente, use o modo "a" (append).

Também é possível escrever múltiplas linhas em um arquivo de texto usando o método `File.write` junto com o caractere especial "\n", que representa uma nova linha.

## Veja também

- [Documentação oficial do Ruby sobre a classe File](https://ruby-doc.org/core-2.7.1/File.html)
- [Exemplos práticos de escrita em arquivos de texto em Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Tutorial sobre abertura e escrita de arquivos em Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)