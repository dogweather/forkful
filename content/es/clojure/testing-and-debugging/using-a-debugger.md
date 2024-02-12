---
title:                "Usando un depurador"
aliases:
- /es/clojure/using-a-debugger.md
date:                  2024-01-26T03:48:33.010311-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/using-a-debugger.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Usar un depurador significa que te estás equipando con una lupa para escudriñar tu código. Los programadores hacen esto para aplastar errores, entender el flujo y asegurarse de que su lógica se desarrolle como esperan.

## Cómo hacerlo:
Clojure se apoya en la Máquina Virtual de Java (JVM), por lo que mucha de la depuración ocurre con herramientas de Java. Una de estas herramientas es `CIDER`, un paquete potente para el desarrollo en Clojure en Emacs, que tiene capacidades de depuración sólidas. Vamos a sumergirnos:

```clojure
;; Primero, inicia sesión en un proyecto Clojure dentro de Emacs usando CIDER
M-x cider-jack-in

;; Establece un punto de interrupción
;; Navega hasta la línea en tu código Clojure que quieras inspeccionar y
;; presiona "C-c M-b" o ejecuta:
M-x cider-debug-defun-at-point

;; Cuando el código se ejecute, llegarás al punto de interrupción. CIDER te pedirá con:
;; 1. n para ir al siguiente paso lógico en la ejecución,
;; 2. c para continuar la ejecución hasta el siguiente punto de interrupción,
;; 3. q para dejar de depurar.

;; Inspecciona las variables locales en el punto de interrupción
;; Mientras estés en un punto de interrupción, escribe:
locals

;; Verás una lista de variables locales y sus valores impresa en el minibuffer.
```
La salida de muestra puede parecer:
```clojure
{:x 10, :y 20, :result 200}
```

## Inmersión Profunda
El depurador es una herramienta tan antigua como las colinas en términos informáticos. El término "bug" (error) fue acuñado en los primeros días de la informática cuando un insecto real causó un error al cortocircuitar un circuito en una máquina.

Aunque `CIDER` es genial para los entusiastas de Emacs, hay alternativas para la depuración en Clojure. Por ejemplo, usar IntelliJ con el plugin Cursive puede ofrecer una experiencia de depuración más guiada por una interfaz gráfica de usuario. Además, puedes usar el Leiningen incorporado o tools.deps para controlar el flujo del proceso al depurar.

Bajo el capó, estos depuradores a menudo manipulan bytecodes, realizan evaluaciones en sesiones nREPL dedicadas y ofrecen inspección de trazas de pila. Están aprovechando las capacidades de la JVM subyacente, accediendo a la riqueza de los marcos de depuración de Java.

## Ver También
- [Documentación del Depurador de CIDER](https://docs.cider.mx/cider/debugging/debugger.html)
- [Depurador de Cursive](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen para Automatización y Depuración](https://leiningen.org/)
- [tools.deps.alpha para más control](https://github.com/clojure/tools.deps.alpha)
