---
title:    "Python: 乱数の生成"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ
ランダムな数字を生成するプログラミングを行う理由は、様々な目的に使用できるからです。例えば、ゲームやシミュレーション、暗号学などにおいて、ランダムな値を使用する必要があります。また、ランダムな値を生成することで、プログラムの動作をテストすることもできます。

## 方法
乱数を生成するには、Pythonの標準ライブラリであるrandomモジュールを使用します。以下のコードを使用することで、乱数を生成することができます。"

```Python
import random
num = random.randint(1, 100) # 1から100までの乱数を生成
print(num) # 乱数を出力
```

上記の例では、1から100までの乱数を生成していますが、生成する範囲は任意の値に変更することができます。また、乱数を重複させたくない場合には、randomモジュールの中にあるsample()関数を使用することができます。

```Python
import random
num_list = [1, 2, 3, 4, 5]
random_num = random.sample(num_list, 3) # num_listから3つの乱数を重複せずに生成してリストとして返す
print(random_num) # 例: [3, 5, 1]
```

さらに、randomモジュールには乱数を生成するためのさまざまな関数が用意されていますので、ドキュメントを参照することでより詳細な情報を得ることができます。

## ディープダイブ
乱数を生成する方法について、もっと深く掘り下げてみましょう。乱数を生成するアルゴリズムには様々な種類がありますが、Pythonのrandomモジュールでは、メルセンヌツイスターというアルゴリズムを使用しています。メルセンヌツイスターは高速であり、周期も非常に長いという特徴があります。また、メルセンヌツイスターの他にも、様々なアルゴリズムがあるので、興味のある方は調べてみると良いでしょう。

## 関連記事
- [Python公式ドキュメント - randomモジュール](https://docs.python.org/ja/3/library/random.html)
- [Python randomモジュールを使ってみよう](https://qiita.com/creeper3eee/items/1a60c787cc7e22302c97)
- [乱数の生成方法について](https://www.edemy.jp/technologies/1108)