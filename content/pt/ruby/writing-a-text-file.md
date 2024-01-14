---
title:                "Ruby: Escrevendo um arquivo de texto"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto é uma habilidade fundamental para programadores Ruby, pois permite que você armazene e manipule dados de forma eficiente. Além disso, arquivos de texto são uma forma simples e universal de compartilhar informações entre diferentes sistemas.

## Como fazer?

Para escrever um arquivo de texto em Ruby, você pode seguir os seguintes passos:

1. Abra um novo arquivo usando o método File.new:
```
texto = File.new("meu_arquivo.txt", "w")
```
2. Em seguida, use o método puts para escrever o conteúdo desejado no arquivo:
```
texto.puts("Este é o meu primeiro arquivo de texto escrito em Ruby!")
```
3. Por fim, feche o arquivo utilizando o método close:
```
texto.close
```
Pronto! Agora você tem um arquivo de texto criado e escrito em Ruby.

## Profundando um pouco mais

Existem também outras formas de escrever em arquivos de texto em Ruby, como por exemplo o uso dos métodos print e printf. Além disso, é importante lembrar de sempre tratar possíveis erros ao escrever em arquivos, utilizando as declarações de exceção (try/catch).

## Veja também

Para mais informações sobre escrita de arquivos de texto em Ruby, confira os seguintes links:

- Documentação oficial do Ruby: https://ruby-doc.org/core-2.7.2/File.html
- Tutorial de escrita de arquivos em Ruby: http://zetcode.com/lang/rubytutorial/files/
- Exemplos de código no GitHub: https://github.com/vgaidarji/ruby-file