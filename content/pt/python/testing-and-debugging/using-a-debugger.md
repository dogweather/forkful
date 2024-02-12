---
title:                "Usando um depurador"
aliases:
- /pt/python/using-a-debugger/
date:                  2024-01-26T04:08:54.084302-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/using-a-debugger.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
"Usar um depurador" significa percorrer o seu código Python para descobrir erros e entender o comportamento. Fazemos isso porque é muito mais fácil do que apenas adivinhar onde as coisas deram errado, e nos poupa horas do purgatório de instruções print.

## Como Fazer:
Vamos detalhar como usar o `pdb`, o depurador embutido do Python. Imagine um arquivo, `buggy.py`, com um erro difícil de encontrar:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

Ao executar este script, você espera `8`, mas ele apenas gera um erro de sintaxe. É hora do depurador!

No seu terminal, execute:
```bash
python -m pdb buggy.py
```

Você entrará no depurador, e ele se parece com isso:
```Python
> /caminho_para_o_arquivo/buggy.py(1)<module>()
-> def add_one(number):
```

Use `l(ist)` para ver mais código, `n(ext)` para ir para a próxima linha, ou `c(ontinue)` para continuar executando o script. Quando você encontrar o erro, o `pdb` vai parar e permitir que você inspecione.

Após corrigir `number ++ 1` para `number + 1`, reinicie o depurador para testar a correção.
Lembre-se, amigos não deixam amigos programar sem uma rede de segurança. É isso.

## Mergulho Profundo
Nos Tempos das Trevas da programação (conhecidos como antes dos ambientes de desenvolvimento integrados, ou IDEs, estarem em todo lugar), os depuradores eram frequentemente ferramentas independentes que você usaria fora do seu editor de texto. Eles vinham para o resgate permitindo que os programadores inspecionassem o estado do seu software em vários pontos de execução.

Em 2023, o `pdb` do Python não é o único recurso disponível. As pessoas podem usar IDEs como PyCharm ou Visual Studio Code, que têm seus próprios depuradores elegantes integrados. Eles adicionam recursos úteis como pontos de interrupção que você pode definir com um clique, em vez de digitar comandos crípticos.

Depois, tem o `ipdb`, um pacote que pode ser instalado via pip e traz as maravilhas do `IPython` para a depuração. É como o `pdb` com esteroides, com auto-completar e destaque de sintaxe.

Os depuradores também variam na sua implementação. Alguns se aproximam e interagem diretamente com a execução do programa no nível da máquina ou do código de bytes. Outros, como muitos depuradores de linguagens de alto nível, executam o código em um ambiente especial que monitora os estados das variáveis e controla o fluxo de execução.

## Veja Também
Para conhecer todos os detalhes do depurador do Python, confira:
- A documentação do `pdb`: https://docs.python.org/3/library/pdb.html

Se você está curioso sobre alternativas, estes links serão úteis:
- Repositório e guia de uso do `ipdb`: https://github.com/gotcha/ipdb
- Depurando com o Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- Recursos de depuração do PyCharm: https://www.jetbrains.com/help/pycharm/debugging-code.html

Feliz caça aos bugs!
