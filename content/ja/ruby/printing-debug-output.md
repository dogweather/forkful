---
title:                "Ruby: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングでデバッグ出力を行う理由をご紹介します。デバッグ出力は、コードの実行中に発生したエラーや想定しない動作を追跡するのに役立ちます。また、コードの実行を理解するためにも重要です。

## 方法

デバッグ出力を行うためには、`puts`メソッドを使用します。これは、コードが実行されると、指定した変数や文をターミナルに出力します。例を見てみましょう。

```Ruby
name = "あかり"
puts "私の名前は#{name}です。"
```

上記のコードを実行すると、ターミナルには「私の名前はあかりです。」という出力が表示されます。

## ディープダイブ

デバッグ出力は、特定の部分のコードがどのように実行されているかを詳しく追跡するのに役立ちます。`puts`メソッドを使用することで、変数の値や条件分岐の結果などを確認することができます。これにより、問題の原因を特定するのに役立ちます。

また、デバッグ出力を行う際には、`p`メソッドも重宝します。`p`メソッドは、デバッグ用の出力に特化した`puts`メソッドです。変数の値やオブジェクトの状態を詳しく出力してくれるので、より詳細なデバッグが可能です。

## 参考リンク

- [Ruby 公式ドキュメント: デバッグ出力](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-puts)
- [Ruby 公式ドキュメント: pメソッド](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-p)
- [Rubyにおけるデバッグ方法の基本](https://qiita.com/jnchito/items/42193d066bd61c740612)
- [Rubyのデバッグ方法をマスターする](https://blog.cloud-acct.com/posts/ruby_develop/222)