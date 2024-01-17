---
title:                "ランダムな数を生成する"
html_title:           "Fish Shell: ランダムな数を生成する"
simple_title:         "ランダムな数を生成する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 何をしていますか？
プログラマーが乱数を生成するのは、ランダムなデータを作成するためです。これは、様々な用途に使用されます。例えば、ゲームやシミュレーション、セキュリティのテストなどです。

# 方法：
```Fish Shell ... ```コードブロック内にコーディング例とサンプルの出力があります。

```bash
# 乱数の生成
echo (random)

# 範囲を指定して乱数を生成
echo (random 1 10)

# 整数のみを生成
echo (random -i)

# 複数の乱数を生成
echo (random 1 100 5)

# 10桁の乱数を生成
echo (random -c 10)

# 16進数の乱数を生成
echo (random -x)
```

出力例：

```bash
0.702657  # デフォルトの0から1までの浮動小数点数を返します。
8         # 1から10までのランダムな整数を返します。
88334     # 5つの乱数を生成します。
2638987235 # 10桁の乱数を生成します。
abfa      # 16進数の乱数を返します。
```

# 深堀：
乱数生成は、科学技術計算や統計学において古くから利用されてきました。かつては、物理学者や数学者が手動で乱数表を作成していましたが、現在ではコンピューターを使用して生成することが一般的です。

代替方法としては、ソフトウェアベースの乱数ジェネレーターの他に、ハードウェアベースのジェネレーターがあります。ハードウェアベースのジェネレーターは、コンピューターの内部状態に依存せず、外部の物理的な現象を使用して乱数を生成することができます。

Fish Shellでは、

# 他の情報
- [Fish Shellの公式ドキュメント] (https://fishshell.com/docs/current/commands.html#random)
- [乱数ジェネレーターについての詳細な説明] (https://en.wikipedia.org/wiki/Random_number_generation)