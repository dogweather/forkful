---
date: 2024-01-26 03:48:29.507864-07:00
description: "\u65B9\u6CD5\uFF1A Clojure\u306FJava Virtual Machine\uFF08JVM\uFF09\u306B\
  \u4F9D\u5B58\u3057\u3066\u3044\u308B\u305F\u3081\u3001\u591A\u304F\u306E\u30C7\u30D0\
  \u30C3\u30B0\u306FJava\u30C4\u30FC\u30EB\u3067\u884C\u308F\u308C\u307E\u3059\u3002\
  \u305D\u306E\u3088\u3046\u306A\u30C4\u30FC\u30EB\u306E\u4E00\u3064\u304C`CIDER`\u3067\
  \u3001Emacs\u5185\u3067\u306EClojure\u958B\u767A\u306E\u305F\u3081\u306E\u30D1\u30EF\
  \u30FC\u30CF\u30A6\u30B9\u30D1\u30C3\u30B1\u30FC\u30B8\u3067\u3042\u308A\u3001\u5805\
  \u7262\u306A\u30C7\u30D0\u30C3\u30B0\u6A5F\u80FD\u3092\u6301\u3063\u3066\u3044\u307E\
  \u3059\u3002\u3055\u3042\u3001\u6F5C\u308A\u307E\u3057\u3087\u3046\uFF1A."
lastmod: '2024-04-05T21:53:42.511012-06:00'
model: gpt-4-0125-preview
summary: ''
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
