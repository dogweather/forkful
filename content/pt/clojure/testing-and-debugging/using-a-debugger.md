---
title:                "Usando um depurador"
aliases: - /pt/clojure/using-a-debugger.md
date:                  2024-01-26T03:48:18.335639-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/using-a-debugger.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?
Usar um depurador significa que você está se equipando com uma lupa para examinar seu código. Os programadores fazem isso para eliminar bugs, entender o fluxo e garantir que sua lógica funcione conforme o esperado.

## Como:
Clojure se apoia na Máquina Virtual Java (JVM), então muito da depuração acontece com ferramentas Java. Uma dessas ferramentas é o `CIDER`, um pacote poderoso para desenvolvimento em Clojure no Emacs, que possui capacidades sólidas de depuração. Vamos mergulhar:

```clojure
;; Primeiro, conecte-se a um projeto Clojure dentro do Emacs usando o CIDER
M-x cider-jack-in

;; Defina um ponto de interrupção
;; Navegue até a linha no seu código Clojure que você quer inspecionar e
;; pressione "C-c M-b" ou execute:
M-x cider-debug-defun-at-point

;; Quando o código for executado, você alcançará o ponto de interrupção. O CIDER irá lhe oferecer:
;; 1. n para ir para o próximo passo lógico na execução,
;; 2. c para continuar a execução até o próximo ponto de interrupção,
;; 3. q para sair da depuração.

;; Inspecione as variáveis locais no ponto de interrupção
;; Enquanto estiver em um ponto de interrupção, digite:
locals

;; Você verá uma lista de variáveis locais e seus valores impressos no minibuffer.
```
A saída de exemplo pode parecer com:
```clojure
{:x 10, :y 20, :result 200}
```

## Aprofundando
O depurador é uma ferramenta tão antiga quanto as colinas em termos de computação. O termo "bug" foi cunhado nos primeiros dias da computação, quando um inseto real causou um erro ao provocar um curto-circuito em uma máquina.

Embora o `CIDER` seja ótimo para entusiastas do Emacs, existem alternativas para a depuração Clojure. Por exemplo, usar o IntelliJ com o plugin Cursive pode oferecer uma experiência de depuração orientada por GUI. Além disso, você pode usar o Leiningen embutido ou tools.deps para controlar o fluxo do processo durante a depuração.

Por debaixo do capô, esses depuradores frequentemente manipulam bytecodes, realizam avaliações em sessões nREPL dedicadas e oferecem inspeção de rastreamento de pilha. Eles estão aproveitando as capacidades da JVM subjacente, explorando a riqueza dos frameworks de depuração do Java.

## Veja Também
- [Documentação do Depurador CIDER](https://docs.cider.mx/cider/debugging/debugger.html)
- [Depurador Cursive](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen para Automação e Depuração](https://leiningen.org/)
- [tools.deps.alpha para mais controle](https://github.com/clojure/tools.deps.alpha)
