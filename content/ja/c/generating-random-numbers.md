---
title:                "ランダムな数値の生成"
html_title:           "C: ランダムな数値の生成"
simple_title:         "ランダムな数値の生成"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ
なぜ乱数を生成することが重要なのか？それについて簡単に説明します。

乱数を生成することは、様々な目的で使用されます。例えば、ゲームのランダムな敵の配置や、暗号の作成、ランダムなデータの生成などです。乱数を利用することで、予測不可能性を持った多様なデータを手軽に生成できます。

## 使い方
以下に、C言語で乱数を生成する方法を示します。コードの詳細な説明は省略し、実際に動作する例を示します。

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {

    // 乱数の種を現在時刻に設定
    srand(time(NULL));

    // 0から10までの乱数を生成
    int random_number = rand() % 11;

    // 生成した乱数を出力
    printf("ランダムな数字：%d", random_number);

    return 0;
}
```

実行結果は以下のようになります。

```
ランダムな数字：7
```

C言語では、```rand()```関数によって乱数が生成されます。この関数は、srand()関数で乱数の種を設定することで、毎回異なる乱数が生成されるようになります。また、```%```演算子を使用することで、特定の範囲内の乱数を生成することができます。

## 詳細を掘り下げる
乱数を生成するために使用される様々なアルゴリズムがありますが、C言語では線形合同法と呼ばれる方法が用いられています。これは、組み込みの数学関数を使用して、前の乱数から新しい乱数を計算することで実現されます。

乱数の種を設定することで、同じプログラムを実行しても同じ乱数の系列が生成されるようになります。しかし、種を指定しない場合は現在時刻が自動的に乱数の種として設定されるため、異なる乱数が生成されます。

## さらに学ぶ
以上の方法以外にも、様々な方法で乱数を生成することができます。興味のある方は、以下のリンクを参考に、より詳細な情報を学ぶことができます。

- [乱数生成アルゴリズムの比較](https://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0%E7%94%9F%E6%88%90%E3%82%A2%E3%83%AB%E3%82%B4%E3%83%AA%E3%82%BA%E3%83%A0%E3%81%AE%E6%AF%94%E8%BC%83)
- [rand()関数の仕様](https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_71/rtref/rand.htm)
- [C言語における乱数の生成](https://learnc.info/c/random_numbers.html)

---

## 関連リンク
- [C言語リファレンス](https://ja.wikibooks.org/wiki/C%E8%A8%80%E8%AA%9E%E3%83%AA%E3%83%95%E3%82%A1%E3%83%AC%E3%83%B3%E3%82%B9)
- [C言語入門](https://www.studytonight