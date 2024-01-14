---
title:    "Ruby: ランダム数字の生成"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# なぜランダムな数字を生成するのか

ランダムな数字を生成することは、コンピュータプログラミングにおいて非常に重要です。例えば、ゲームやシミュレーション、セキュリティなどの分野でランダムな要素が必要になる場合があります。Rubyには、簡単にランダムな数字を生成するための便利なメソッドが用意されています。

## 生成方法

ランダムな数字を生成するためには、以下のように```rand```メソッドを使用します。

```Ruby
rand(max) 
```

このメソッドは、0から```max```までの範囲のランダムな整数を返します。例えば、```rand(10)```とすると、0から9までの範囲の数値がランダムに出力されます。また、小数を含むランダムな数値を生成するには、以下のように```rand```メソッドを使用します。

```Ruby
rand(max).to_f 
```

この場合、小数点以下の桁数は自動的に適切に丸められます。

また、特定の範囲の中からランダムに数字を取得したい場合には、以下のように```rand```メソッドに範囲を指定します。

```Ruby
rand(min..max) 
```

このように指定することで、```min```から```max```までの範囲からランダムに数値を取得することができます。

## 詳細を深く掘り下げる

実際にRubyのランダム数字を生成するメソッドである```rand```の処理を深く掘り下げると、以下のようになっています。

```Ruby
def rand(n = nil) 
  if n 
    Kernel::rand(n)
  else 
    Kernel::rand 
  end 
end 
```

このメソッドはデフォルトで```Kernel::rand```を呼び出しますが、引数に数値が与えられた場合には、その数値を何かの式のように評価するために```Kernel::rand(n)```を呼び出します。この式内では、ほとんどの場合で数値を足し引きしてから結果を得るため、さらに内部で使用した計算はランダムになります。

## See Also

- [Rubyドキュメンテーション(日本語)](https://docs.ruby-lang.org/ja/latest/method/Kernel/m/rand.html)
- [Rubyでランダムに値を取得する方法](https://qiita.com/na2ken/items/ae01ca4254623972ef69)
- [ゲーム開発におけるランダム性の重要性とその実装方法(C#での解説ですが、概念的には共通です)](https://tech.playground-gc.net/rnd/)