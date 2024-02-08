---
title:                "デバッガーの使い方"
aliases:
- ja/clojure/using-a-debugger.md
date:                  2024-01-26T03:48:29.507864-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガを使用するということは、コードを精査するための拡大鏡を自分に装備するということです。プログラマーはこれを行い、バグを潰し、フローを理解し、ロジックが期待通りに展開されるかを確かめます。

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
