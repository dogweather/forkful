---
date: 2024-01-26 04:17:27.324824-07:00
description: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u3001\
  \u307E\u305F\u306FREPL (Read-Eval-Print Loop) \u306F\u3001\u30EA\u30A2\u30EB\u30BF\
  \u30A4\u30E0\u3067\u30B3\u30FC\u30C9\u3092\u30C6\u30B9\u30C8\u3067\u304D\u308B\u3088\
  \u3046\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u5B8C\u5168\u306A\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u3053\
  \u3068\u306A\u304F\u3001Ruby\u306E\u30CB\u30E5\u30A2\u30F3\u30B9\u3092\u5B9F\u9A13\
  \u3001\u30C7\u30D0\u30C3\u30B0\u3001\u305D\u3057\u3066\u5B66\u7FD2\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:16.407194-06:00'
model: gpt-4-0125-preview
summary: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u3001\
  \u307E\u305F\u306FREPL (Read-Eval-Print Loop) \u306F\u3001\u30EA\u30A2\u30EB\u30BF\
  \u30A4\u30E0\u3067\u30B3\u30FC\u30C9\u3092\u30C6\u30B9\u30C8\u3067\u304D\u308B\u3088\
  \u3046\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u5B8C\u5168\u306A\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u3053\
  \u3068\u306A\u304F\u3001Ruby\u306E\u30CB\u30E5\u30A2\u30F3\u30B9\u3092\u5B9F\u9A13\
  \u3001\u30C7\u30D0\u30C3\u30B0\u3001\u305D\u3057\u3066\u5B66\u7FD2\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
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
