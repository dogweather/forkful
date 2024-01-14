---
title:    "Ruby: デバッグ出力の表示"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## なぜ
なぜデバッグ出力を表示する必要があるのか、という疑問があるかもしれません。しかし、プログラミングをする際には、コードがどのように動作しているかを理解するためにデバッグ出力が非常に役立ちます。エラーの原因を特定したり、プログラム全体の流れを把握するのに役立つのです。

## 方法
デバッグ出力を表示する方法はとても簡単です。Rubyでは`puts`メソッドを使用して、出力したい文字列をコンソールに表示することができます。例えば、`puts "Hello World"`というコードを実行すると、コンソールに"Hello World"という文字列が表示されます。

```Ruby
# デバッグ出力の例
def add_numbers(x, y)
  puts "最初の数字は#{x}です"
  puts "2番目の数字は#{y}です"

  result = x + y
  puts "計算結果は#{result}です"
end

# メソッドを呼び出す
add_numbers(5, 10)
```

上記の例では、メソッド内で`puts`メソッドを使用して、変数の値や計算結果をコンソールに表示しています。

## 詳細を調べる
デバッグ出力をより詳しく調べる方法として、`p`メソッドがあります。`p`メソッドを使用すると、オブジェクトの型情報も含めてコンソールに表示されるため、どのようなデータが変数に格納されているかをより詳細に確認することができます。

```Ruby
# デバッグ出力の例
users = ["John", "Jane", "Mike"]
p users
```

実行すると、コンソールに`["John", "Jane", "Mike"]`という出力が表示されます。これは配列データであることがわかりますが、`p`メソッドを使用しない場合は、配列の内容が表示されるだけで型情報は表示されません。

## 関連記事
デバッグ出力については、他にも様々な方法やテクニックがあります。以下のリンクを参考に、さまざまな観点からデバッグ出力を学んでみてください。

- [Ruby 公式ドキュメント - `puts`メソッドの説明](https://docs.ruby-lang.org/ja/latest/method/Kernel/v/puts.html)
- [Ruby 公式ドキュメント - `p`メソッドの説明](https://docs.ruby-lang.org/ja/latest/method/Kernel/v/p.html)
- [Ruby Magic - デバッグ出力のベストプラクティス](https://pragmaticstudio.com/blog/2014/3/11/putting-the-p-in-the-pry)
- [Codecademy - デバッグ出力の使い方を学ぶ](https://www.codecademy.com/articles/how-to-debug-or) 

セーアルソー
- よく使われるデバッグツール - [Pry](https://github.com/pry/pry)
- オンラインでコードをデバッグするツール - [Repl.it](https://repl.it/)
- `puts`メソッドのアルティメットガイド - [Debugging with puts() - The Ultimate Guide](https://natashatherobot.com/debugging-with-puts/)