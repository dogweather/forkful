---
title:    "Bash: Escrevendo testes."
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes no Bash é importante?

Escrever testes é uma prática essencial para qualquer programador. No Bash, não é diferente. Testes garantem que o código está funcionando como esperado, evita erros e facilita a manutenção do código no futuro.

## Como escrever testes no Bash?

Existem diferentes formas de escrever testes no Bash, mas aqui vão duas opções comuns:

1. Usando o comando `test`:

```
#!/bin/bash
# Teste com o comando test
test -e arquivo.txt # Verifica se o arquivo existe
if [ $? -eq 0 ]; then # Checa se o código de saída foi 0 (verdadeiro)
  echo "Arquivo encontrado!"
else
  echo "Arquivo não encontrado."
fi
```

2. Usando o comando `[[`:

```
#!/bin/bash
# Teste com o comando [[
if [[ -e arquivo.txt ]]; then # Verifica se o arquivo existe
  echo "Arquivo encontrado!"
else
  echo "Arquivo não encontrado."
fi
```

É importante notar que no Bash, espaços e sinais de pontuação são significativos, então certifique-se de seguir a sintaxe correta para que seus testes funcionem corretamente.

## Profundidade: Escrevendo bons testes no Bash

Para escrever testes efetivos, é importante ter uma boa cobertura de código. Isso significa que seus testes devem abranger todas as possíveis situações e caminhos do seu código. Além disso, é importante manter seus testes atualizados conforme o seu código é alterado.

Outra dica é nomear seus testes adequadamente para facilitar a identificação dos problemas em caso de falha. Além disso, é interessante utilizar ferramentas de cobertura de código para verificar quais partes do seu código não estão sendo testadas e, assim, garantir uma cobertura completa.

## Veja também

- [Guia de referência do Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Documentação do comando test](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [Test Driven Development (TDD) no Bash](https://technology.riotgames.com/news/test-driven-development-bash-scripting)