---
title:                "Gleam: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Por que escrever um arquivo de texto em Gleam

 Se você está no mundo da programação, provavelmente já ouviu falar de Gleam, uma linguagem de programação funcional e estática projetada para construir aplicativos escaláveis e confiáveis. Apesar de ser voltada para desenvolvedores web, Gleam também é uma ótima opção para trabalhar com arquivos de texto. Mas você deve estar se perguntando: por que eu deveria escrever um arquivo de texto em Gleam? Aqui estão algumas razões:

- Gleam é uma linguagem altamente eficiente e com uma sintaxe fácil de entender, o que pode simplificar o processo de escrita de arquivos de texto.
- Como Gleam é uma linguagem estática, ela oferece uma forte verificação de tipo, garantindo que o arquivo de texto esteja estruturado corretamente desde o início.
- Além disso, o uso de Gleam pode ajudar na criação de scripts mais robustos e resistentes a erros, tornando seu trabalho muito mais fácil e eficiente.

Agora que entendemos por que escrever um arquivo de texto em Gleam pode ser benéfico, vamos ver como podemos fazer isso!

# Como fazer isso em Gleam

Para escrever um arquivo de texto em Gleam, primeiro precisamos importar o módulo `gleam/io` e em seguida, utilizar a função `File.write` para criar o arquivo e salvar nosso conteúdo nele. Veja um exemplo abaixo:

```
import gleam/io

pub fn escrever_arquivo() {
  content = "Esse é um arquivo de texto escrito em Gleam."
  File.write("arquivo.txt", content)
}
```

O código acima irá criar um arquivo chamado "arquivo.txt" com o conteúdo especificado na variável `content`. Se quisermos adicionar mais texto ao arquivo, podemos usar a função `File.append`.

```
File.append("arquivo.txt", "Mais um texto adicionado ao arquivo.")
```

Também podemos trabalhar com variáveis e formatações dentro do conteúdo do arquivo. Veja um exemplo:

```
meu_nome = "Gabriel"
File.write("arquivo.txt", "Olá, meu nome é #{meu_nome}.")
```

O arquivo "arquivo.txt" terá como conteúdo "Olá, meu nome é Gabriel.".

# Aprofundando-se

Escrever um arquivo de texto em Gleam é bastante simples e pode ser feito de forma semelhante a outras linguagens de programação. No entanto, algo interessante é que Gleam também oferece a opção de trabalhar com arquivos UTF-8, permitindo que desenvolvedores criem conteúdos em diferentes línguas e caracteres especiais sem problemas.

Além disso, para facilitar ainda mais, o módulo `gleam/io` fornece outras funções úteis, como `File.read`, para ler o conteúdo de um arquivo, `File.rename`, para renomear um arquivo e `File.remove`, para excluir um arquivo. Você pode ver a documentação completa dessas funções [aqui](https://gleam.run/modules/gleam_io.html).

# Veja também

- Para saber mais sobre a linguagem Gleam, acesse [o site oficial](https://gleam.run/).
- Se quiser se aprofundar na escrita de arquivos em Gleam, confira [este artigo](https://alexinsinga.com/posts/write-read-utf8-text-files-in-gleam/).
- Para encontrar mais recursos sobre Gleam, confira [a lista de pacotes e bibliotecas disponíveis](https://github.com/gleam-extras).

Agora que você sabe como escrever um arquivo de texto em Gleam, aproveite todo o potencial dessa linguagem incrível e crie seus próprios scripts e aplicativos!