---
title:                "Fish Shell: ランダムな数字の生成"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

プログラマーにとって、ランダムな数値を生成することは非常に便利なことです。ランダムな数値を使用することで、様々なゲームやシミュレーション、データ分析などにおいて多様性や偶発性を加えることができます。Fish Shellを使用すると、簡単にランダムな数値を生成することができます。

## 使い方

ランダムな数値を生成するには、`math` コマンドを使用します。例えば、1から10までのランダムな数値を出力する場合は、以下のようにします。

```Fish Shell
math $RANDOM % 10 + 1
```

また、特定の範囲のランダムな数値を複数回生成することもできます。以下の例では、1から5までのランダムな数値を3回出力します。

```Fish Shell
for i in (seq 3)
  echo (math $RANDOM % 5 + 1)
end
```

実行結果は以下のようになります。

```
2
5
1
```

## 詳細を掘り下げる

`math` コマンドを使用する際に、`$RANDOM` 変数を使用することで、ランダムな数値を生成することができます。`$RANDOM` 変数は、各シェルで異なる値を持つ乱数を生成します。また、`math` コマンドには、他にも様々な数学関数や演算子があり、より柔軟なランダム数値の生成が可能です。詳細な情報は、[Fish Shellの公式ドキュメント](https://fishshell.com/docs/current/cmds/math.html)を参照してください。

## 関連リンク

- [Math Command Documentation in Fish Shell](https://fishshell.com/docs/current/cmds/math.html)
- [Generating Random Numbers in Fish Shell](https://medium.com/@mattstratton/generating-random-numbers-in-fish-shell-f825df8c565b)
- [Using the Math Command in Fish Shell](https://www.linuxjournal.com/article/10749)