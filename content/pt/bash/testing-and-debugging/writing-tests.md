---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:35.473067-07:00
description: "Escrever testes em Bash envolve criar scripts de casos de teste para\
  \ validar a funcionalidade dos seus scripts Bash. Os programadores realizam testes\
  \ para\u2026"
lastmod: '2024-03-13T22:44:46.756221-06:00'
model: gpt-4-0125-preview
summary: Escrever testes em Bash envolve criar scripts de casos de teste para validar
  a funcionalidade dos seus scripts Bash.
title: Escrevendo testes
weight: 36
---

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
