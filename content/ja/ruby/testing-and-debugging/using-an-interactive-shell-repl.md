---
date: 2024-01-26 04:17:27.324824-07:00
description: "\u65B9\u6CD5\uFF1A Ruby\u306EREPL\u306FIRB\uFF08Interactive Ruby\uFF09\
  \u3068\u547C\u3070\u308C\u307E\u3059\u3002\u7AEF\u672B\u304B\u3089\u76F4\u63A5Ruby\u3092\
  \u8A66\u3057\u3066\u307F\u307E\u3057\u3087\u3046\uFF1A."
lastmod: '2024-04-05T21:53:43.644499-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

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
