---
title:    "Bash: Escrevendo no erro padrão"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que Escrever para o Erro Padrão?

Escrever para o erro padrão é um recurso importante na programação em Bash. Ele permite que os desenvolvedores enviem mensagens de erro e outras informações para um local específico, em vez de misturar essas informações com a saída do programa. Isso torna a depuração mais fácil e ajuda a criar um código mais organizado e eficiente.

## Como Fazer

Para escrever para o erro padrão em Bash, utilizamos o comando `echo` seguido do símbolo `>` e o número `2`. Por exemplo, se quisermos enviar uma mensagem de erro, podemos usar o seguinte código:

```Bash 
echo "Erro: arquivo não encontrado." > 2
```

Podemos também redirecionar todo o conteúdo do erro padrão para um arquivo específico, utilizando o comando `2>`. Por exemplo:

```Bash
grep "Palavra-chave" arquivo.txt 2> erros.txt
```

Isso irá executar o comando `grep` normalmente, mas todos os erros serão redirecionados para o arquivo "erros.txt".

## Profundidade

Existem várias razões pelas quais pode ser útil escrever para o erro padrão em Bash. Algumas delas incluem:

- Separação de informações: ao enviar mensagens de erro para um local específico, podemos separar essas informações da saída normal do programa, facilitando a leitura e depuração do código.

- Tratamento de erros: escrever para o erro padrão torna mais fácil lidar com erros e exceções em nossos programas. Podemos verificar o arquivo de erros para identificar possíveis problemas e tomar ações específicas para lidar com eles.

- Mensagens personalizadas: podemos personalizar as mensagens de erro que enviamos para o erro padrão, tornando-as mais informativas e amigáveis para o usuário final.

## Ver também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Guia rápido de Bash](https://devhints.io/bash)