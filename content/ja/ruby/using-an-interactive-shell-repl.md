---
title:                "インタラクティブシェル（REPL）の使用"
date:                  2024-01-26T04:17:27.324824-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
インタラクティブシェル、またはREPL (Read-Eval-Print Loop) は、リアルタイムでコードをテストできるようにします。プログラマーは、完全なスクリプトを作成することなく、Rubyのニュアンスを実験、デバッグ、そして学習するためにこれを使用します。

## 方法：
RubyのREPLはIRB（Interactive Ruby）と呼ばれます。端末から直接Rubyを試してみましょう：

```Ruby
irb
2.7.0 :001 > puts "Hello, Ruby world!"
Hello, Ruby world!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## 深掘り
Ruby 1.8で導入されたIRBは、Rubyistの必需品です。LispやPythonのインタラクティブシェルに触発され、実験と即時フィードバックを融合させています。構文ハイライトやより堅牢なデバッグ環境など、より多くの機能を提供するPryのような代替品もあります。IRB自体はシンプルですが、'irbtools'のようなgemを使用して機能を拡張することができます。IRBがread-eval-printループをどのように扱っているかというと、各入力行を読み込み、Rubyコードとして評価し、その結果を印刷し、このプロセスを終了するまでループします。

## 参照
- [RubyのIRB](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [irbtools gem](https://github.com/janlelis/irbtools)
