---
title:                "Ruby: デバッグ出力の印刷"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ
デバッグ出力を表示する理由は、プログラマーがコードが期待通りに動作しているかどうかを確認するためです。エラーが発生した場合、デバッグ出力を使用して問題の原因を特定することができます。

## 方法
デバッグ出力を表示するには、```puts```というメソッドを使います。以下のコード例を参考にしてください。

```Ruby
def add_numbers(x, y)
    puts "Adding #{x} and #{y}!"
    return x + y
end

sum = add_numbers(3, 5)
puts "The total is #{sum}!"
```
出力結果:
```
Adding 3 and 5!
The total is 8!
```

## 深堀り
デバッグ出力には、異なるレベルの詳細度を指定することもできます。例えば、```puts "DEBUG: #{variable}"```とすることで、特定の変数の値を確認することができます。また、```p variable```とすることで、変数のデータ型や値を詳しく表示することができます。

## 参考リンク
- [Rubyのデバッグ出力についてのドキュメント](https://docs.ruby-lang.org/ja/latest/class/IO.html#M_P-puts)
- [Rubyのデバッグ技術についての記事](https://qiita.com/joker1007/items/32ba10235eb383956c52)
- [Rubyのデバッグ出力を上手に使う方法](https://www.codementor.io/@bryanarboleda/using-debug_output-in-ruby-d0qzv3whh)