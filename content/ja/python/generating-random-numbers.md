---
title:    "Python: ランダム数の生成"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することの意義は何でしょうか？プログラミングでランダムな数字を使うことは、ゲームやシミュレーションなどでランダムな要素を取り入れることができるため非常に有用です。また、セキュリティや暗号化にも欠かせない重要な技術です。

## 方法

Pythonでは、`random`モジュールを使用して簡単にランダムな数字を生成することができます。以下のコードを使用して、0から10までのランダムな整数を表示してみましょう。

```Python
import random

# 0から10までの範囲でランダムな整数を生成
num = random.randint(0, 10)

print(num)  # 例：5
```

同様に、小数点以下のランダムな数字を生成するには、`random.uniform()`を使用します。

```Python
import random

# 0から1までの範囲でランダムな小数を生成
num = random.uniform(0, 1)

print(num)  # 例：0.459376
```

## 深堀り

ランダムな数字を生成するアルゴリズムには様々な種類がありますが、Pythonの`random`モジュールでは、メルセンヌ・ツイスター(Mersenne Twister)アルゴリズムを使用しています。これは非常に高い品質のランダムな数字を生成することができるアルゴリズムであり、多くのプログラミング言語で採用されています。

また、Pythonではシード(seed)という値を指定することで、同じランダムな数字を再現することもできます。例えば、以下のように実行すると、毎回同じ結果が表示されます。

```Python
import random

# シードを指定
random.seed(1)

# 0から10までの範囲でランダムな整数を生成
num = random.randint(0, 10)

print(num)  # 例：9

# 同じシードを指定しても同じ結果が表示される
random.seed(1)
num = random.randint(0, 10)

print(num)  # 例：9
```

## 参考リンク

- [Python公式ドキュメント：randomモジュール](https://docs.python.org/ja/3/library/random.html)
- [Pythonでランダムな整数を生成する方法](https://techacademy.jp/magazine/18891)
- [メルセンヌ・ツイスター法の解説](https://www.slideshare.net/ssuser1863f7/random-49092919)

## この記事が気に入ったら？

この記事が気に入ったら、ぜひ以下のリンクもチェックしてください。

- [Pythonでランダムな文字列を生成する方法](https://qxresearch-nagoya.com/python-random/)
- [Pythonでランダムな要素を含むリストを作成する方法](https://lespros.co.jp/media/articles/323)
- [Pythonで乱数を使ってゲームを作るチュートリアル](https://qiita.com/gragragrao/items/1bf96616d33940f42075)

ありがとうございました！