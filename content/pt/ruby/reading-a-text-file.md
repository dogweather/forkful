---
title:                "Ruby: Lendo um arquivo de texto"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Por que ler um arquivo de texto?

Ler um arquivo de texto é uma habilidade fundamental em programação, especialmente em Ruby. Isso permite que você acesse e manipule dados armazenados em um arquivo. Além disso, é uma tarefa comumente exigida em muitos projetos de programação.

## Como fazer

Para ler um arquivo de texto em Ruby, você pode seguir os seguintes passos:

1. Abra o arquivo usando o comando `File.open` e passe o nome do arquivo e o modo de leitura como parâmetros. Por exemplo: `file = File.open("arquivo.txt", "r")`.
2. Use o método `read` para ler todo o conteúdo do arquivo e armazená-lo em uma variável. Por exemplo: `conteudo = file.read`.
3. Se desejar, você pode imprimir o conteúdo usando o método `puts` ou fazer qualquer outra manipulação de dados desejada.

Aqui está um exemplo completo de como ler e imprimir o conteúdo de um arquivo de texto:

```
file = File.open("arquivo.txt", "r")
conteudo = file.read
puts conteudo
```

Se o arquivo estiver em uma localização diferente do seu script Ruby, você precisará fornecer o caminho completo para o arquivo.

## Uma imersão mais profunda

Ao lidar com arquivos de texto em Ruby, é importante lembrar alguns pontos:

- Certifique-se de fechar o arquivo depois de ler ou escrever nele usando o método `close`. Isso garante que todos os recursos do sistema operacional alocados para o arquivo sejam liberados.
- Se você precisa ler o conteúdo de um arquivo em um formato específico, como JSON ou CSV, pode usar bibliotecas externas, como `json` ou `csv`, que facilitam a leitura e a manipulação desses tipos de arquivos.
- Use o bloco `File.open()` para garantir que o arquivo seja fechado automaticamente após o seu uso.

# Veja também

- [Documentação oficial do Ruby sobre o método File.open](https://ruby-doc.org/core-2.7.0/File.html#method-c-open)
- [Tutorial da DigitalOcean sobre leitura e escrita em arquivos com Ruby](https://www.digitalocean.com/community/tutorials/how-to-work-with-files-in-ruby-pt)
- [Guia da Codecademy sobre manipulação de arquivos em Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-io/cheatsheet)