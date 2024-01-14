---
title:    "PHP: Escrevendo para o erro padrão"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever no standard error
Escrever no standard error é uma prática comum na programação PHP. Ao enviar mensagens de erro para o standard error, você pode facilmente identificar os problemas em seu código para resolvê-los de forma eficiente. Isso é especialmente útil em projetos maiores, onde há muitas linhas de código e pode ser difícil identificar a causa de determinados erros.

## Como fazer
Para escrever no standard error em PHP, você pode utilizar a função `fwrite()`. Ela permite que você escreva diretamente no standard error, fornecendo uma mensagem de erro personalizada. Veja um exemplo de código abaixo:

```PHP
$mensagem = "Ocorreu um erro na linha 10!";
fwrite(STDERR, $mensagem);
```

Ao executar esse código, a mensagem será enviada para o standard error e será exibida na tela do terminal ou do servidor. Isso ajudará a identificar o erro e facilitará a correção do código.

## Aprofundando-se
Escrever no standard error é uma forma de melhorar a depuração e o controle de erros no seu código PHP. Além disso, ao usar essa técnica, você pode imprimir mensagens de erro personalizadas ao lidar com exceções no seu código.

É importante lembrar que o standard error é diferente do standard output (utilizado na função `echo`). Enquanto o standard output é usado para mensagens de sucesso, o standard error é destinado a mensagens de erro. Portanto, é essencial usá-los corretamente para uma depuração eficiente.

## Veja também
- [Documentação oficial da função fwrite](https://www.php.net/manual/en/function.fwrite.php)
- [Como lidar com erros em PHP](https://www.php.net/manual/en/errorfunc.configuration.php)