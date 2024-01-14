---
title:                "Fish Shell: 「ランダムな数字の生成」"
simple_title:         "「ランダムな数字の生成」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

 `## Why`

乱数を生成する理由はさまざまです。例えば、シミュレーションやランダムなデータを必要とするゲーム開発などに使用することができます。
 
 
## 使い方

 `## How To`

ランダムな数値を生成するには、以下のコードを使います。

```
Fish Shellを使用したランダム数値生成の例：

progress bar 0 100 | for i in (range 10); echo (math random 0 100) && sleep 1; end

出力例：
27.3986
55.1652
82.6743
12.6754
...

```

このコードでは、0から100までのランダムな数値を10個生成し、1秒ごとに出力されます。このように、`math random`コマンドを使用することで、特定の範囲内でランダムな数値を生成することができます。

また、`crypto random`コマンドを使用することでもランダムなデータを生成することができます。例えば、以下のようなコードを実行するとランダムなパスワードを生成することができます。

```
Fish Shellを使用したランダムパスワード生成の例：

set -l chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
set new_pass ""
for i in (seq 1 20)
	set i (math --random index (count $chars))
	set new_pass $new_pass$chars[$i]
end
echo $new_pass

出力例：
zG0rCiz2SPVh3R1QT6U4
```

上記のコードでは、`set`コマンドを使用してランダムな文字列から20文字のパスワードを生成しています。

## 深堀り

 `## Deep Dive`

乱数を生成する際には、いくつかの注意点があります。まず、乱数は完全にランダムではなく、特定のパターンが存在する可能性があります。そのため、セキュリティや暗号化などの重要な分野では、`crypto random`コマンドを使用することが推奨されています。

また、コード内で乱数を使用する際には、重複が生じないように注意が必要です。そのため、乱数を生成し終わったらリセットするなどの処理が必要です。さらに、特定の数字が出現しやすいという偏りも存在するため、複数の乱数を生成してそれらをランダムに組み合わせることで偏りを補正することができます。

## 併せて参照

`See Also`

- Fish Shell公式ドキュメント (https://fishshell.com/docs/current/cmds/math.html) 
- ランダムなパスワードを生成する方法 (https://www.shellhacks.com/fish-generate-random-password/)

この記事を参考に、ぜひ乱数生成を使って便利なアプリケーションやゲームを開発してみてください！