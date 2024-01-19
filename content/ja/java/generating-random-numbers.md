---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ? (What & Why?)

ランダムな数字の生成は、予測可能なパターンを作らない新しい番号の作成を意味します。プログラマーはこれを行うことで、テストの多様性を高めたり、意図しない偏りを減らすことができます。

## 手順 (How to):

以下のコードブロックでは、Javaのテクニックを使用してランダム数字の生成方法を示します。

```Java
import java.util.Random;

public class Main {
    public static void main(String[] args) {
        Random rand = new Random();

        int num = rand.nextInt(50);
        System.out.println("生成されたランダムな数値は: " + num);
    }
}
```

上記のコードを実行すると、出力は次のようになります（値はランダムに変わるため、毎回変わります）：

```Java
生成されたランダムな数値は: 37
```


## ディープダイブ (Deep Dive):

過去には、ランダムな数字を生成するための他の方法が一般的でした。例えば、`Math.random()`関数は、`java.util.Random()`コンストラクタが導入される前によく使用されていました。しかし、現代では`java.util.Random()`がより優れた選択となっています。これは、複数の並列タスクでの性能低下を防ぐためのThreadLocalRandom、よりに高いエントロピーのRandomなど、より多くの選択肢が提供されているためです。

## 参考資料 (See Also):

関連情報については以下をご覧ください:

- Oracle公式ドキュメントにおけるRandomクラス: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- StackOverFlowにおけるランダムな数値生成に関する議論: https://stackoverflow.com/questions/363681/how-do-i-generate-random-integers-within-a-specific-range-in-java