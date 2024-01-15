---
title:                "ランダムな数値の生成"
html_title:           "Java: ランダムな数値の生成"
simple_title:         "ランダムな数値の生成"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

乱数を生成することには様々な用途があります。例えば、ランダムなデータを用いてプログラムのテストやシミュレーションを行うことができます。また、乱数を使ってランダムな選択やゲームを実装することもできます。乱数を生成するプログラムを作ることで、より多様な機能を持つプログラムを作ることができます。

## 方法

ランダムな整数を生成するには、Javaの`Math`クラスにある`random()`メソッドを使用します。このメソッドは、0から1未満の乱数を返します。

```Java
public class RandomNumberGenerator {
    public static void main(String[] args) {
        // 0から1未満の乱数を生成する
        double randomNumber = Math.random();

        // 乱数を整数に変換する
        int randomInt = (int) (randomNumber * 100);

        // 0から99までの乱数を出力する
        System.out.println(randomInt);
    }
}
```

上記のコードを実行すると、0から99までのランダムな整数が出力されます。

また、範囲を指定してランダムな整数を生成するには、`Random`クラスを使用します。このクラスには、`nextInt()`メソッドがあり、引数として範囲を指定することができます。

```Java
public class RandomNumberGenerator {
    public static void main(String[] args) {
        // 0から10までの範囲で乱数を生成する
        Random random = new Random();
        int randomNumber = random.nextInt(10);

        // 0から10までの乱数を出力する
        System.out.println(randomNumber);
    }
}
```

上記のコードを実行すると、0から10までのランダムな整数が出力されます。

## ディープダイブ

乱数を生成する際、計算される乱数は疑似乱数であり、実際には完全にランダムではありません。`Math.random()`メソッドでは、乱数のシード値を設定することができないため、再現性のある乱数を生成することはできません。しかし、`Random`クラスではシード値を設定することができます。シード値を設定することで、同じシード値を使用すると同じ乱数の系列が得られるため、再現性のある乱数を生成することができます。

また、乱数を生成する際は、乱数の分布にも注意する必要があります。`Math.random()`メソッドや`Random`クラスでは、一様分布の乱数を生成することができますが、他の分布の乱数を生成するには特殊なアルゴリズムやライブラリを使用する必要があります。

## おわりに

もしランダムなデータや数値が必要な場合は、Javaで乱数を生成する方法がとても簡単であることが分かりました。乱数を使うことで、より多様な機能を持つプログラムを作ることができるので、ぜひ試してみてください。

## 関連リンク

- [Java Math