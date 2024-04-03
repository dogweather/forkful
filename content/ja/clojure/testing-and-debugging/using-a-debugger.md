---
date: 2024-01-26 03:48:29.507864-07:00
description: "Clojure\u306FJava Virtual Machine\uFF08JVM\uFF09\u306B\u4F9D\u5B58\u3057\
  \u3066\u3044\u308B\u305F\u3081\u3001\u591A\u304F\u306E\u30C7\u30D0\u30C3\u30B0\u306F\
  Java\u30C4\u30FC\u30EB\u3067\u884C\u308F\u308C\u307E\u3059\u3002\u305D\u306E\u3088\
  \u3046\u306A\u30C4\u30FC\u30EB\u306E\u4E00\u3064\u304C`CIDER`\u3067\u3001Emacs\u5185\
  \u3067\u306EClojure\u958B\u767A\u306E\u305F\u3081\u306E\u30D1\u30EF\u30FC\u30CF\u30A6\
  \u30B9\u30D1\u30C3\u30B1\u30FC\u30B8\u3067\u3042\u308A\u3001\u5805\u7262\u306A\u30C7\
  \u30D0\u30C3\u30B0\u6A5F\u80FD\u3092\u6301\u3063\u3066\u3044\u307E\u3059\u3002\u3055\
  \u3042\u3001\u6F5C\u308A\u307E\u3057\u3087\u3046\uFF1A\u2026"
lastmod: '2024-03-13T22:44:41.564628-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u306FJava Virtual Machine\uFF08JVM\uFF09\u306B\u4F9D\u5B58\u3057\
  \u3066\u3044\u308B\u305F\u3081\u3001\u591A\u304F\u306E\u30C7\u30D0\u30C3\u30B0\u306F\
  Java\u30C4\u30FC\u30EB\u3067\u884C\u308F\u308C\u307E\u3059\u3002\u305D\u306E\u3088\
  \u3046\u306A\u30C4\u30FC\u30EB\u306E\u4E00\u3064\u304C`CIDER`\u3067\u3001Emacs\u5185\
  \u3067\u306EClojure\u958B\u767A\u306E\u305F\u3081\u306E\u30D1\u30EF\u30FC\u30CF\u30A6\
  \u30B9\u30D1\u30C3\u30B1\u30FC\u30B8\u3067\u3042\u308A\u3001\u5805\u7262\u306A\u30C7\
  \u30D0\u30C3\u30B0\u6A5F\u80FD\u3092\u6301\u3063\u3066\u3044\u307E\u3059\u3002\u3055\
  \u3042\u3001\u6F5C\u308A\u307E\u3057\u3087\u3046\uFF1A\n\n```clojure\n;; \u307E\u305A\
  \u3001Emacs\u5185\u3067CIDER\u3092\u4F7F\u7528\u3057\u3066Clojure\u30D7\u30ED\u30B8\
  \u30A7\u30AF\u30C8\u306B\u63A5\u7D9A\u3057\u307E\u3059\nM-x cider-jack-in\n\n;;\
  \ \u30D6\u30EC\u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\u3092\u8A2D\u5B9A\n;; \u691C\u67FB\
  \u3057\u305F\u3044Clojure\u30B3\u30FC\u30C9\u306E\u884C\u306B\u79FB\u52D5\u3057\u3001\
  \n;; \"C-c M-b\"\u3092\u62BC\u3059\u304B\u3001\u6B21\u3092\u5B9F\u884C\u3057\u307E\
  \u3059\uFF1A\nM-x cider-debug-defun-at-point\n\n;; \u30B3\u30FC\u30C9\u304C\u5B9F\
  \u884C\u3055\u308C\u308B\u3068\u3001\u30D6\u30EC\u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\
  \u306B\u5230\u9054\u3057\u307E\u3059\u3002CIDER\u306F\u4EE5\u4E0B\u3092\u6C42\u3081\
  \u307E\u3059\uFF1A\n;; 1."
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

## 方法：
ClojureはJava Virtual Machine（JVM）に依存しているため、多くのデバッグはJavaツールで行われます。そのようなツールの一つが`CIDER`で、Emacs内でのClojure開発のためのパワーハウスパッケージであり、堅牢なデバッグ機能を持っています。さあ、潜りましょう：

```clojure
;; まず、Emacs内でCIDERを使用してClojureプロジェクトに接続します
M-x cider-jack-in

;; ブレークポイントを設定
;; 検査したいClojureコードの行に移動し、
;; "C-c M-b"を押すか、次を実行します：
M-x cider-debug-defun-at-point

;; コードが実行されると、ブレークポイントに到達します。CIDERは以下を求めます：
;; 1. n は実行の次の論理ステップに進むため、
;; 2. c は次のブレークポイントまで実行を続けるため、
;; 3. q はデバッグを終了するため。

;; ブレークポイントでローカル変数を検査
;; ブレークポイントにいる間、次のようにタイプします：
locals

;; minibufferにローカル変数とその値のリストが表示されます。
```
サンプル出力は次のようになるかもしれません：
```clojure
{:x 10, :y 20, :result 200}
```

## 深掘り
デバッガは、コンピューティング用語で言えば古き良き時代からあるツールです。「バグ」という用語は、実際の昆虫が機械の回路をショートさせてエラーを引き起こしたコンピューティングの初期の日々に遡ります。

`CIDER`はEmacs愛好者には素晴らしいですが、Clojureデバッグのための代替手段もあります。たとえば、IntelliJとCursiveプラグインを使用すると、よりGUI駆動のデバッグ体験を得ることができます。さらに、組み込みのLeiningenやtools.depsを使用してデバッグ時のプロセスフローを制御することができます。

内部では、これらのデバッガはしばしばバイトコードを操作し、専用のnREPLセッションで評価を行い、スタックトレースの検査を提供します。彼らはJVMの下層の機能を活用し、Javaのデバッグフレームワークの豊富なリソースにアクセスしています。

## 参照
- [CIDERデバッガドキュメント](https://docs.cider.mx/cider/debugging/debugger.html)
- [Cursiveデバッガ](https://cursive-ide.com/userguide/debugging.html)
- [自動化とデバッグのためのLeiningen](https://leiningen.org/)
- [より多くの制御のためのtools.deps.alpha](https://github.com/clojure/tools.deps.alpha)
