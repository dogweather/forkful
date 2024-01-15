---
title:                "Escrevendo testes"
html_title:           "Bash: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Por que

Se você é novo no mundo da programação ou já está envolvido em desenvolvimento há algum tempo, é provável que já tenha ouvido falar sobre a importância de escrever testes. Mas por que se preocupar em escrever testes para o seu código? A resposta é simples: testes são essenciais para garantir que o seu código funcione corretamente e evita possíveis erros e bugs no futuro. Além disso, escrever testes pode ajudar a melhorar a qualidade do seu código e a facilitar a manutenção no longo prazo.

## Como fazer

Escrever testes não é uma tarefa difícil, mas requer um pouco de prática e conhecimento básico de como o Bash funciona. Aqui estão alguns passos para ajudá-lo a começar a escrever testes eficientes em Bash.

Primeiro, é importante identificar qual parte do seu código precisa de testes. Em outras palavras, quais são as funções, comandos ou trechos de código mais críticos que precisam ser testados? Uma vez identificado isso, você pode começar a escrever testes específicos para essas partes do seu código.

Aqui está um exemplo de como escrever um teste simples para uma função em Bash que soma dois números:

```Bash
#!/bin/bash

# Definindo uma função que soma dois números
sum() {
  local result=$(( $1 + $2 ))
  echo "Resultado: $result"
}

# Teste: soma de dois números positivos
sum_result=$(sum 5 10)
if [ "$sum_result" != "Resultado: 15" ]; then
  echo "Teste falhou: soma de 5 e 10 não produziu o resultado esperado!"
  exit 1
fi
echo "Teste passou: soma de 5 e 10 produziu o resultado esperado."
```

Neste exemplo, criamos uma função chamada `sum` que recebe dois parâmetros e soma os valores, e em seguida verificamos se o resultado está correto utilizando um teste simples. Se o resultado não for o esperado, o teste falha e encerra o script com status de saída 1. Caso contrário, o teste é considerado bem sucedido e o script continua sua execução normalmente.

## Profundidade

Aqui estão algumas dicas adicionais para escrever testes em Bash:

- Utilize o comando `set -e` no início do seu script para garantir que ele sai imediatamente se algum comando falhar. Isso ajuda a prevenir problemas futuros no seu código.
- Uma alternativa para o comando `set -e` é usar `set -u`, que sai imediatamente se alguma variável não estiver definida. Isto pode ser útil para evitar erros relacionados a variáveis não inicializadas.
- Ao escrever um teste, é importante também testar casos de uso inválidos e limites. Isso garante que o seu código seja robusto e capaz de lidar com diferentes cenários.
- Utilize ferramentas como `grep`, `awk` e `sed` para verificar a saída de algum comando e garantir que ela está correta. Esses utilitários podem ser muito úteis ao escrever testes mais avançados.

## Veja também

Aqui estão algumas referências adicionais sobre escrever testes em Bash:

- [Bash Unit Testing Guide](https://stevens.netmeister.org/631/bash_unit_testing.html)
- [Effective Testing in Bash – Part I](https://spin.atomicobject.com/2018/05/15/effective-testing-bash-part-1/)
- [Building a test framework with Bash](https://www.mnielsen.org/blog/building-a-test-framework-with-bash/)