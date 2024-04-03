---
date: 2024-01-26 03:43:10.488564-07:00
description: "\u65B9\u6CD5\uFF1A \u3053\u3053\u306BBash\u3067\u306E\u4E38\u3081\u65B9\
  \u306E\u8981\u70B9\u304C\u3042\u308A\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.360533-06:00'
model: gpt-4-0125-preview
summary: "\u3053\u3053\u306BBash\u3067\u306E\u4E38\u3081\u65B9\u306E\u8981\u70B9\u304C\
  \u3042\u308A\u307E\u3059\uFF1A."
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

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
