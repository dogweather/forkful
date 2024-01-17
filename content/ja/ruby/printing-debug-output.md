---
title:                "デバッグ出力のプリント"
html_title:           "Ruby: デバッグ出力のプリント"
simple_title:         "デバッグ出力のプリント"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## なに？なぜ？
デバッグ出力とは何か、なぜプログラマーがそれを行うのかを説明します。デバッグ出力は、プログラムの実行中にその状態を把握するための重要な手段です。プログラムの実行中にデバッグ出力を追加することで、必要な箇所を特定して問題を解決することができます。

## 方法：
下記はデバッグ出力を行うためのコード例及び出力例です。 ```Ruby ... ``` コードブロック内に記載されています。

```
# コード例
def multiply(x, y)
  puts "xの値は#{x}です。" # 変数の値を出力
  puts "yの値は#{y}です。"
  return x * y
end

puts multiply(2, 3)

# 出力例
xの値は2です。
yの値は3です。
6
```

## ディープダイブ：
デバッグ出力は、プログラム開発の歴史の中で重要な役割を果たしています。以前は、コンソールにメッセージを出力することが主な方法でしたが、現在ではログファイルやデバッガーなどさまざまな手段があります。また、バグを取り除くためにデバッグ出力を使用する方法の代わりに、ユニットテストやテストドリブン開発の手法があります。Rubyでは、```puts```や```p```、```print```などのメソッドを使用してデバッグ出力を行うことができます。

## 関連リンク：
- [Rubyのデバッグ出力についてのドキュメント](https://docs.ruby-lang.org/ja/latest/doc/debug_documentation.html)
- [デバッグ出力の歴史](https://blog.nelhage.com/2008/05/a-brief-history-of-debugging/)
- [テストドリブン開発についての記事](https://www.tdi.co.jp/miso/testdriventutorial.html)