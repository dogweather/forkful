---
title:                "Escrevendo testes"
aliases:
- /pt/google-apps-script/writing-tests/
date:                  2024-02-01T22:08:34.351988-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo testes"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/writing-tests.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Escrever testes no Google Apps Script (GAS) refere-se à criação de scripts automatizados para verificar o comportamento de seus códigos, garantindo que eles funcionem como esperado sob várias condições. Programadores fazem isso para capturar bugs cedo, melhorar a qualidade do código e facilitar atualizações e manutenção mais fácil.

## Como Fazer:

Embora o Google Apps Script não tenha um framework de testes integrado como alguns outros ambientes de programação, você ainda pode escrever e executar testes aproveitando funções simples do GAS ou integrando bibliotecas de testes externas, como o `QUnit`. Aqui está um exemplo básico usando uma função simples do GAS para testar outra função em seu script:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Teste falhou: add(2, 3) deveria ser 5, mas foi " + result);
  } else {
    Logger.log("Teste passou!");
  }
}
```

Executar `testAdd()` vai registrar "Teste passou!" se a função `add` funcionar corretamente, ou lançar um erro se não funcionar. Para uma abordagem mais sofisticada, integrar o QUnit com o Google Apps Script envolve mais alguns passos, mas oferece um ambiente de testes poderoso. Um exemplo de configuração de teste com o QUnit é assim:

1. Inclua a biblioteca QUnit em seu projeto.
2. Crie um arquivo HTML de teste para executar os testes do QUnit.
3. Escreva casos de teste usando a sintaxe do QUnit.

Aqui está um exemplo usando o QUnit:

```javascript
// Inclua o QUnit vinculando-o em um arquivo HTML usado para executar seus testes

QUnit.test("Testando a função add", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) deve retornar 5");
});
```

Para ver os resultados, abra o arquivo HTML dentro do Editor de Scripts GAS ou implante-o como um aplicativo web.

## Mergulho Profundo

Historicamente, testar no Google Apps Script tem sido um tanto negligenciado, provavelmente devido às origens da plataforma e aos casos de uso primários focados em tarefas de automação rápidas e de pequena escala, em vez de grandes aplicações. Como tal, o GAS não oferece os mesmos frameworks e ferramentas de teste robustas encontradas em ambientes de programação mais tradicionais. No entanto, a comunidade se adaptou, incorporando bibliotecas de código aberto e aproveitando criativamente as ferramentas existentes do Google.

Usar bibliotecas como o QUnit representa um grande passo à frente, mas vem com seu próprio conjunto de desafios, como configurar um ambiente de teste adequado e aprender uma sintaxe adicional. No entanto, para aqueles investidos em construir aplicações mais complexas e confiáveis com o GAS, o esforço vale a pena.

Alternativas, como usar funções simples do GAS para testes, oferecem facilidade de uso e integração com o ambiente GAS sem dependências adicionais, mas carecem de recursos de teste abrangentes e a capacidade de escalar facilmente conforme seu projeto cresce. Ferramentas como o clasp (a Interface de Linha de Comando do Google Apps Script) podem facilitar fluxos de trabalho mais avançados, incluindo testes, permitindo que os desenvolvedores codifiquem em seus IDEs preferidos, introduzindo espaço para integrar com frameworks de teste externos de forma mais harmoniosa.

Em conclusão, enquanto o GAS pode não ter suporte nativo para testes sofisticados prontos para uso, sua flexibilidade e as abordagens inovadoras da comunidade fornecem caminhos viáveis ​​para garantir que seus scripts sejam robustos, confiáveis ​​e preparados para qualquer tarefa.
