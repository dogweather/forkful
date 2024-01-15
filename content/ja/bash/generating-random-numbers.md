---
title:                "ランダムナンバーの生成"
html_title:           "Bash: ランダムナンバーの生成"
simple_title:         "ランダムナンバーの生成"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することにどのような意味があるのか疑問に思ったことはありませんか？実は、ランダムな数字を生成することは多くのゲームやシミュレーション、暗号化など様々なアプリケーションで重要な役割を果たしています。

## 生成方法

Bashを使ってランダムな数字を生成するには、`$RANDOM`変数を使用します。この変数は0から32767までのランダムな整数を生成します。

```Bash
echo $RANDOM
```

上記のコマンドを実行すると、毎回異なる数が表示されることが確認できるでしょう。また、範囲を指定することも可能です。例えば、10から100までのランダムな数を生成するには以下のようにします。

```Bash
echo $((RANDOM%91+10))
```

さらに、シェルスクリプト内でランダムな数を使用する場合は、次のように変数に代入することもできます。

```Bash
number=$((RANDOM%50+50))
echo "ランダムな数は$numberです。"
```

ターミナル上で試してみると、毎回異なる範囲の数が表示されることがわかります。

## ディープダイブ

ランダムな数字を生成するには、ストリームから生成される疑似乱数を使用します。詳しい仕組みは深く掘り下げると長くなってしまいますが、簡単に言えば、プログラミング言語やオペレーティングシステムに組み込まれたアルゴリズムによって生成される数の並びです。

しかし、これらの疑似乱数は本当のランダム性を持たず、実際は初期値やアルゴリズムによって決まる周期性を持っています。そのため、暗号化などセキュリティに関わる場合には注意が必要です。真のランダム性を実現するには、外部デバイスから入力を受け取り、その値を使用する方法などがあります。

## 参考リンク

- [Bashの$RANDOM変数の詳細](https://www.gnu.org/software/bash/manual/html_node/Special-Parameters.html)
- [シェルスクリプトでランダムな数値を生成する方法](https://linuxhint.com/generate_random_num_bash/)
- [ランダム数列の生成ソースコード](https://www.phiresky.de/random-numbers-from-dev-random-in-bash)
- [セキュリティに関わる場合には注意が必要な疑似乱数の使用](https://resources.infosecinstitute.com/prng-vs-csprng-specifics/#gref)

## 関連リンク