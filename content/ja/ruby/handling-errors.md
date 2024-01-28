---
title:                "エラー処理"
date:                  2024-01-26T00:57:21.132586-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/handling-errors.md"
---

{{< edit_this_page >}}

## 何となぜ？

エラー処理とは、予期せぬコード内のエラーを上手く扱ったり、クラッシュせずに問題を管理することです。プログラマーは、正常に動作しないときにフローをコントロールし、ユーザー体験を滑らかにするためにこれを行います。

## 方法：

Rubyでは`begin`、`rescue`、`ensure`、および`end`を用いてエラーを処理します。リスクのあるコードを`begin`と`end`で囲みます。エラーが発生した場合、`rescue`が機能します。

```Ruby
begin
  # リスクのあるコードをここに書く。
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "Oops! それはできません: #{e.message}"
ensure
  puts "これは常に実行されます、エラーがあるかないかに関わらず。"
end
```

サンプル出力：
```
Oops! それはできません: 0で除算しました
これは常に実行されます、エラーがあるかないかに関わらず。
```

## 詳細解説

歴史的に、プログラミング言語におけるエラー処理は大きく進化し、初期の言語には粗末であったり存在しないメカニズムもありました。Rubyの例外処理は、PythonやSmalltalkのような言語によって刺激を受けています。

Rubyにおける`begin-rescue`の代替法としては、メソッド定義内で`rescue`を使うか、標準的なエラー処理には使われないが、非標準のフロー制御のために`throw`と`catch`を使う方法があります。

興味深い詳細の一つに、Rubyの例外はオブジェクト（`Exception`クラスおよびその子孫のインスタンス）であることがあります。従って、カスタムエラークラスを定義し、単にエラーをログに記録する以上のことを行うことができます。これにより、より堅牢なエラー処理のためにプログラム内の豊かな状態を運ぶことができます。

## 関連情報

- Rubyの例外とエラー処理に関するドキュメント: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Rubyエラー処理に関するベストプラクティスの詳細なガイド: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
