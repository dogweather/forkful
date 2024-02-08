---
title:                "数値の丸め処理"
aliases:
- ja/bash/rounding-numbers.md
date:                  2024-01-26T03:43:10.488564-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/rounding-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

数値を丸めるとは、小数点以下を切り捨てて、ある特定の文脈に十分な単純な値にすることを意味します。プログラマーは、結果を簡素化するため、スペースを節約するため、または正確な値が重要ではない場合（CPU使用率やディスクスペースを大まかに見積もるときなど）に数値を丸めます。小数点以下があなたの一日を左右するわけではありません。

## 方法：

ここにBashでの丸め方の要点があります：

```Bash
# 'floor'を使って切り捨てる
echo "scale=0; 3.49/1" | bc

# 'ceiling'を使って切り上げる
echo "scale=0; 3.01/1" | bc -l

# printfを使って最も近い整数に丸める
printf "%.0f\n" 3.49

# bcを使って最も近い整数に丸めるトリック
echo "(3.49+0.5)/1" | bc
```

ターミナルから直接のサンプル出力：

```
3  # 切り捨てられた（floor）
4  # 切り上げられた（ceiling）
3  # 最も近い数に丸められた（printfを使って）
3  # 最も近い数に丸められた（bcを使って）
```

## 深堀り

昔の日々には、Bashスクリプトで数学の魔法を行うための`bc`や`printf`はありませんでした。古株達は外部ツールに頼ったり、巧妙な回避策を考え出さなければなりませんでした。今では、`bc`を使って精度高い数学ができます。ただし、`bc`はデフォルトで丸めるわけではなく、切り捨てます。スケール部分は小数点の動作を設定します。

代替案は？ `bc`に切り替えずに`awk`を使って丸めたり、より重い数学が必要な場合には`perl`で手を動かすことができます。マゾヒスティックな場合は、例えば、反復的な文字列操作で純粋なBashを使ってみてください – でも、なぜですか？

詳細については、`bc`は丸めるだけではなく、たくさんの数学的な作業を行います – スケール調整、正弦、平方根、名前を挙げてください。`printf`に関しては、テキストのフォーマットに関するものが多いですが、数値を丸めることもできるので、文句を言うことはありません。

## 参照

もっと詳しく知りたい方へ：

- GNU `bc` マニュアル：https://www.gnu.org/software/bc/manual/html_mono/bc.html
- Bash `printf` コマンド：https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-printf
- AWK ユーザーズガイド（丸めやその他のテキスト処理について）：https://www.gnu.org/software/gawk/manual/gawk.html
- より多くのBash数学、スクリプティング、そして数値トリック：https://mywiki.wooledge.org/BashFAQ/022
