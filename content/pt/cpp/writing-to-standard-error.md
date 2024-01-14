---
title:    "C++: Escrevendo para erro padrão"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão, também conhecido como saída de erro, é uma ferramenta útil para os programadores. Quando ocorrem erros durante a execução do código, geralmente é mais fácil e conveniente visualizar essas informações na tela do que procurá-las em arquivos de log.

## Como fazer:

Você pode escrever para o erro padrão usando a função prinft() da biblioteca padrão em C++. Por exemplo, o código abaixo irá imprimir a mensagem "Erro: Divisão por zero" caso o usuário digite um valor de 0 para o denominador:

```C++
int numerador = 10;
int denominador;

cin >> denominador;

if(denominador == 0){
    printf("Erro: Divisão por zero");
}
else{
    int resultado = numerador / denominador;
    printf("Resultado: %d", resultado);
}
```

No exemplo acima, a mensagem de erro será exibida na tela caso ocorra uma divisão por zero, enquanto que, caso não ocorra nenhum erro, o resultado da operação será impresso.

## Aprofundando:

Além da função printf(), existem outras maneiras de escrever para o erro padrão em C++. Por exemplo, a função cerr, também da biblioteca padrão, é especialmente útil para imprimir erros ou mensagens de aviso. Ela funciona da mesma forma que a função printf(), mas imprime na saída de erro ao invés de na saída padrão.

Outro ponto importante a ser mencionado é que, ao utilizar a função printf(), é possível formatar a mensagem de erro de acordo com as especificações do programador, o que torna essa ferramenta ainda mais poderosa para identificar e corrigir erros no código.

## Veja também:

- [Documentação oficial da função printf()](https://www.cplusplus.com/reference/cstdio/printf/)
- [Tutorial sobre saída de erro em C++](https://www.learncpp.com/cpp-tutorial/more-debugging-output-using-stdcerr/)
- [Artigo sobre lidando com erros em C++](https://www.techopedia.com/definition/15480/error-handling)