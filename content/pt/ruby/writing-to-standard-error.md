---
title:                "Ruby: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma técnica útil para entender e depurar o seu código em Ruby. Quando um erro ocorre durante a execução do programa, ele é registrado no erro padrão, também conhecido como STDERR. Ao escrever para o erro padrão, você pode personalizar as mensagens de erro e obter informações valiosas sobre o fluxo de execução do seu código.

## Como fazer?

Existem duas maneiras de escrever para o erro padrão em Ruby: usando o método `puts` ou o método `warn`. Ambos imprimirão a mensagem na tela, mas o método `warn` também exibirá os detalhes do arquivo, linha e método onde o erro ocorreu.

**Exemplo 1: Usando o método `puts`**

```Ruby
# Código de exemplo
numero = 5
if numero > 10
  puts "O número é maior que 10!"
else
  puts "O número é menor ou igual a 10."
end

# Saída esperada caso o número seja 5
O número é menor ou igual a 10.
```

**Exemplo 2: Usando o método `warn`**

```Ruby
# Código de exemplo
numero = "cinco"
if numero.is_a?(Integer)
  puts "O número é um inteiro."
else
  warn "O número não é um inteiro."
end

# Saída esperada caso o número seja "cinco"
O número não é um inteiro.
<main>:3:in `block in <main>': O número não é um inteiro. (RuntimeError)
```

## Mergulho Profundo

Ao escrever para o erro padrão, é importante ter em mente algumas boas práticas. Primeiro, certifique-se de que sua mensagem de erro seja clara e descritiva, para que você possa entender facilmente o que deu errado. Além disso, tente adicionar informações extras, como o contexto do erro ou uma sugestão de solução.

Outra dica importante é evitar escrever em excesso para o erro padrão. Isso pode poluir a saída e tornar mais difícil encontrar mensagens relevantes de erro. Use apenas quando realmente necessário.

Por fim, é importante lembrar que as mensagens de erro escritas para o erro padrão também podem ser capturadas e tratadas em um bloco de `rescue` em seu código. Isso pode ser útil para lidar com possíveis erros de forma mais elegante e controlada.

## Veja também

- [Documentação oficial do método `puts`](https://ruby-doc.org/core-2.6.3/IO.html#method-i-puts)
- [Documentação oficial do método `warn`](https://ruby-doc.org/core-2.6.3/Kernel.html#method-i-warn)
- [Artigo sobre tratamento de erros em Ruby (em inglês)](https://www.rubyguides.com/2019/04/ruby-exceptions/)
- [Vídeo tutorial sobre uso do erro padrão em Ruby (em português)](https://www.youtube.com/watch?v=2r5sfzMlKxE)