---
date: 2024-01-26 03:48:15.191496-07:00
description: "Utiliser un d\xE9bogueur signifie que vous vous \xE9quipez d'une loupe\
  \ pour scruter votre code. Les programmeurs font cela pour \xE9craser les bugs,\
  \ comprendre le\u2026"
lastmod: 2024-02-19 22:05:16.185289
model: gpt-4-0125-preview
summary: "Utiliser un d\xE9bogueur signifie que vous vous \xE9quipez d'une loupe pour\
  \ scruter votre code. Les programmeurs font cela pour \xE9craser les bugs, comprendre\
  \ le\u2026"
title: "Utilisation d'un d\xE9bogueur"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Utiliser un débogueur signifie que vous vous équipez d'une loupe pour scruter votre code. Les programmeurs font cela pour écraser les bugs, comprendre le flux et s'assurer que leur logique se déroule comme prévu.

## Comment faire :
Clojure repose sur la Machine Virtuelle Java (JVM), donc beaucoup de débogage se fait avec des outils Java. Un de ces outils est `CIDER`, un package puissant pour le développement Clojure dans Emacs, qui possède de solides capacités de débogage. Plongeons-nous dedans :

```clojure
;; D'abord, connectez-vous à un projet Clojure dans Emacs en utilisant CIDER
M-x cider-jack-in

;; Mettez un point d'arrêt
;; Naviguez jusqu'à la ligne de votre code Clojure que vous souhaitez inspecter et
;; appuyez sur "C-c M-b" ou exécutez :
M-x cider-debug-defun-at-point

;; Lorsque le code s'exécute, vous atteindrez le point d'arrêt. CIDER vous proposera :
;; 1. n pour passer à l'étape logique suivante dans l'exécution,
;; 2. c pour continuer l'exécution jusqu'au prochain point d'arrêt,
;; 3. q pour arrêter le débogage.

;; Inspectez les locaux au point d'arrêt
;; Lorsqu'à un point d'arrêt, tapez :
locals

;; Vous verrez une liste des variables locales et de leurs valeurs imprimées dans le minibuffer.
```
Un exemple de sortie peut ressembler à :
```clojure
{:x 10, :y 20, :result 200}
```

## Plongée en profondeur
Le débogueur est un outil aussi vieux que les collines en termes informatiques. Le terme "bug" a été inventé au début de l'informatique quand un insecte réel a causé une erreur en court-circuitant un circuit dans une machine.

Bien que `CIDER` soit excellent pour les enthousiastes d'Emacs, il existe des alternatives pour le débogage de Clojure. Par exemple, l'utilisation de IntelliJ avec le plugin Cursive peut offrir une expérience de débogage plus orientée GUI. De plus, vous pouvez utiliser Leiningen intégré ou tools.deps pour contrôler le flux du processus lors du débogage.

Sous le capot, ces débogueurs manipulent souvent des bytecodes, effectuent des évaluations dans des sessions nREPL dédiées et offrent l'inspection de la pile d'appels. Ils exploitent les capacités sous-jacentes de la JVM, tapant dans la richesse des cadres de débogage de Java.

## Voir Aussi
- [Documentation du Débogueur CIDER](https://docs.cider.mx/cider/debugging/debugger.html)
- [Débogueur Cursive](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen pour l'Automatisation et le Débogage](https://leiningen.org/)
- [tools.deps.alpha pour plus de contrôle](https://github.com/clojure/tools.deps.alpha)
