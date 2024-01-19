---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何となぜ？

デバッグ出力の印刷は、コードがどのように動作しているか理解するための手段です。プログラマはそれを使用して、中断したり、誤った出力を生成したり、予期しない動作をするコードを追跡します。

## 使い方:

Rubyでのデバッグ出力は`puts`や`print`、`p`を使うことが一般的です。それぞれ少し異なる動作をします。

```Ruby
# これは3つの結果をコンソールに印刷します
puts "Hello"
print "World"
p "!"

# 出力:
# Hello
# World!
```

しかし、大規模なプログラムやエラーの追跡には、Rubyの組み込みライブラリ`debug`が有効です。

```Ruby
require 'debug'

x = 2
y = 3
puts "Adding these two values..."
Debugger.start
x + y
```

## 深層情報.

### (1) 历史的背景
デバッグ出力は古くからプログラムの問題を解決する一般的な方法で、Rubyでも同様です。

### (2) 代わりの方法
Rubyにはputsやprint、p等のデバッグ用の組み込み関数以外にも、特に大規模なプロジェクトや複雑なバグ対策には`debug`や`awesome_print`等のライブラリが活用されます。

### (3) 実装の詳細
`puts`、`print`、`p`はそれぞれ標準出力に異なる形式で値を出力します。`puts`は改行を出力しますが、`print`は改行しません。`p`は`inspect`メソッド経由でオブジェクトをより詳細に出力します。

## 関連情報:

1. Rubyのオフィシャルな[ドキュメンテーション](https://www.ruby-lang.org/ja/documentation/)
2. デバッグの[基本](https://guides.rubyonrails.org/debugging_rails_applications.html)について
3. [Ruby Debugging Magic Cheat Sheet](https://www.rubyguides.com/2015/06/ruby-debugging/)