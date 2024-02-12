---
title:                "Escrevendo testes"
aliases:
- /pt/bash/writing-tests/
date:                  2024-02-03T19:29:35.473067-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo testes"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Escrever testes em Bash envolve criar scripts de casos de teste para validar a funcionalidade dos seus scripts Bash. Os programadores realizam testes para garantir que seus scripts funcionem conforme esperado sob várias condições, capturando erros e bugs antes do lançamento.

## Como fazer:
O Bash não possui uma estrutura de testes embutida, mas você pode escrever funções de teste simples. Para testes mais sofisticados, ferramentas de terceiros como `bats-core` são populares.

### Exemplo Básico de Teste em Bash Puro:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "Teste passou."
    return 0
  else
    echo "Teste falhou. Esperava-se '$expected_output', obteve-se '$result'"
    return 1
  fi
}

# Invocando a função de teste
test_example_function
```
Saída de Amostra:
```
Teste passou.
```

### Usando `bats-core` para Testes:
Primeiro, instale `bats-core`. Isso geralmente pode ser feito através do seu gerenciador de pacotes ou clonando seu repositório.

Em seguida, escreva seus testes em arquivos `.bats` separados.

```bash
# Arquivo: example_function.bats

#!/usr/bin/env bats

@test "testar exemplo de função" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
Para executar seus testes, simplesmente execute o arquivo `.bats`:
```bash
bats example_function.bats
```
Saída de Amostra:
```
 ✓ testar exemplo de função

1 teste, 0 falhas
```

Essa abordagem permite que você integre facilmente os testes no seu fluxo de trabalho de desenvolvimento, garantindo a confiabilidade e estabilidade dos seus scripts Bash.
