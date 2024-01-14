---
title:    "Gleam: Lendo um arquivo de texto"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Porquê

Existem muitas razões para querer aprender a ler arquivos de texto em Gleam. Se você é um desenvolvedor de software, pode ser necessário analisar dados de arquivos de texto para seu aplicativo ou projeto. Se você é um estudante ou entusiasta de programação, pode querer aprender uma nova linguagem de programação e entender como ela manipula arquivos de texto. Seja qual for o motivo, este post vai te ensinar como ler arquivos de texto em Gleam.

## Como Fazer

Para começar a ler arquivos de texto em Gleam, você precisará seguir alguns passos básicos:

1. Importe o módulo `gleam/text/file` no seu código.
2. Use a função `file.open` para abrir o arquivo que deseja ler.
3. Use a função `file.read` para ler o conteúdo do arquivo.
4. Feche o arquivo usando a função `file.close` para evitar vazamentos de recursos.

Aqui está um exemplo de código que lê um arquivo de texto chamado "exemplo.txt" e imprime seu conteúdo na tela:

```
Gleam import /gleam/text/file as file

result <- file.open("exemplo.txt")
case result {
  Ok(file) ->
    content <- file.read(file)
    file.close(file)
    io.println(content)
  Error(err) ->
    io.format("Erro ao ler o arquivo: {}", [err])
}
```

O código acima primeiro usa a função `file.open` para abrir o arquivo "exemplo.txt". Em seguida, ele usa a função `file.read` para ler o conteúdo do arquivo e armazená-lo na variável `conteúdo`. Finalmente, o arquivo é fechado usando a função `file.close` e o conteúdo é impresso na tela usando a função `io.println`. Se ocorrer algum erro durante este processo, ele será tratado pelo bloco `Error` e uma mensagem de erro será impressa.

## Mergulho Profundo

Além das funções básicas mencionadas acima, o módulo `gleam/text/file` também possui uma variedade de outras funções que podem ser úteis ao ler arquivos de texto. Algumas delas incluem:

- `file.read_line`: lê uma única linha do arquivo;
- `file.read_all_lines`: lê todas as linhas do arquivo e as armazena em uma lista;
- `file.read_as_binary`: lê o arquivo como uma sequência de bytes;
- `file.read_as_binary_line`: lê uma única linha do arquivo como uma sequência de bytes.

Certifique-se de explorar a documentação do módulo para saber mais sobre essas e outras funções.

## Veja Também

- Documentação do módulo `gleam/text/file`: [link](https://gleam.run/modules/gleam/text/file/)
- Tutorial de Gleam: [link](https://gleam.run/tour/)
- Outros exemplos de código em Gleam: [link](https://github.com/lpil/gleam/tree/master/examples)

Esperamos que este post tenha sido útil para você aprender como ler arquivos de texto em Gleam. Se você tiver alguma dúvida ou sugestão, não hesite em deixar um comentário abaixo ou entrar em contato com a comunidade Gleam. Obrigado pela leitura!