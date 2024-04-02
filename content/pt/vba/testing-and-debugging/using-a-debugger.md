---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:25.660577-07:00
description: "No VBA, o depurador \xE9 parte integrante do Editor do Visual Basic\
  \ (VBE). Veja como voc\xEA pode aproveit\xE1-lo: 1. **Definindo Pontos de Interrup\xE7\
  \xE3o**: Clique\u2026"
lastmod: '2024-03-13T22:44:46.417394-06:00'
model: gpt-4-0125-preview
summary: "No VBA, o depurador \xE9 parte integrante do Editor do Visual Basic (VBE).\
  \ Veja como voc\xEA pode aproveit\xE1-lo: 1. **Definindo Pontos de Interrup\xE7\xE3\
  o**: Clique\u2026"
title: Usando um depurador
weight: 35
---

## Como fazer:
No VBA, o depurador é parte integrante do Editor do Visual Basic (VBE). Veja como você pode aproveitá-lo:

1. **Definindo Pontos de Interrupção**: Clique na margem esquerda ao lado da linha de código que lhe interessa, ou coloque o cursor na linha e pressione F9. Isso indica ao VBA para pausar a execução quando chegar a este ponto.

    ```vb
    Sub ExemploDepuracao()
        Dim contador As Integer
        For contador = 1 To 5
            Debug.Print contador ' Definir ponto de interrupção aqui
        Next contador
    End Sub
    ```

    Quando o código é executado, ele pausará na linha `Debug.Print contador`, permitindo que você inspecione os valores das variáveis.

2. **Passar Para Dentro (F8)**: Com este comando, você executa seu código uma declaração de cada vez, entrando em quaisquer procedimentos chamados. É útil para rastrear como seu código e funções interagem.

3. **Janela de Observação**: Use a Janela de Observação para monitorar os valores das variáveis ou expressões. Se uma variável não estiver no escopo, a Janela de Observação indicará isso. Clique com o botão direito em uma variável > Adicionar Observação.

4. **Janela Imediata (Ctrl+G)**: Esta janela é particularmente útil para testar expressões ou modificar valores de variáveis durante a depuração. Digite `?nomeVariavel` para imprimir o valor atual de uma variável, ou atribua um novo valor com `nomeVariavel = novoValor`.

    ```vb
    ' Na Janela Imediata
    ?contador ' Imprime o valor atual do contador
    contador = 3 ' Define o valor do contador como 3
    ```

5. **Saída de Exemplo**:

    Quando você alcança o ponto de interrupção e executa linha por linha usando F8, a Janela Imediata pode exibir algo assim:

    ```
    contador = 1
    contador = 2
    contador = 3
    ```

    Aqui, consultamos manualmente a variável `contador` após cada iteração.

## Aprofundamento:
O depurador no VBA, embora robusto, faz parte de uma tradição mais ampla de ferramentas de depuração em linguagens de programação, evoluindo significativamente desde seus predecessores iniciais. Introduzido nas primeiras versões do VBA, visava fornecer aos desenvolvedores um conjunto de ferramentas simples, porém poderosas, para inspeção e correção de código. Com o tempo, as melhorias incluíram pontos de interrupção condicionais, capacidades de observação aprimoradas e integração com a interface do Excel para inspeção de dados mais intuitiva.

No entanto, comparado com os Ambientes de Desenvolvimento Integrados (IDEs) modernos, como o Visual Studio ou o Eclipse, as ferramentas de depuração do VBA podem parecer básicas. Esses IDEs modernos oferecem recursos mais sofisticados, como inspeção de variáveis em tempo real, pontos de interrupção avançados e frameworks integrados de testes unitários. Embora essas alternativas ofereçam experiências de depuração mais abrangentes, a simplicidade e direção das ferramentas de depuração do VBA permanecem bem adaptadas ao contexto específico de automação e script dentro das aplicações do Microsoft Office.

Para programadores acostumados com esses ambientes modernos, ajustar-se às ferramentas de depuração do VBA pode exigir uma mudança de abordagem. No entanto, os princípios fundamentais de inspeção de variáveis, avanço através do código e observação do comportamento em tempo de execução são universais. Com prática, o depurador do VBA torna-se uma ferramenta indispensável para garantir que seus scripts de automação desempenhem sem falhas dentro do ecossistema do Office.
