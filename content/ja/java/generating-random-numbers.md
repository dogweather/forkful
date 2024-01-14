---
title:    "Java: 「ランダムな数値を生成する」"
keywords: ["Java"]
---

{{< edit_this_page >}}

## なぜ

乱数を生成することの魅力を少し話してみましょう。乱数は、ゲームやシミュレーション、セキュリティなどの様々な分野で非常に重要です。プログラマーとして、乱数を生成する能力は必要不可欠です。

## 方法

まずは、Javaの`Random`クラスを使用して、乱数を生成する方法を見ていきましょう。

```java
import java.util.Random;

public class RandomGenerator {
    public static void main(String[] args) {
        // 1から10の範囲の乱数を生成する
        Random random = new Random();
        int randomNumber = random.nextInt(10) + 1;
        System.out.println("乱数: " + randomNumber);
    }
}
```

`Random`クラスの`nextInt()`メソッドは、指定した範囲内でランダムな整数を生成します。この例では、10を指定しているので、1から10までの乱数が出力されます。

出力例:

```
乱数: 7
```

また、`Random`クラスの`nextDouble()`メソッドを使用すると、小数点以下の乱数を生成することもできます。

```java
// 0から1までの範囲の乱数を生成する
Random random = new Random();
double randomNumber = random.nextDouble();
System.out.println("乱数: " + randomNumber);
```

出力例:

```
乱数: 0.736712
```

## 深堀り

乱数を生成する方法として、`Random`クラス以外にも`Math.random()`メソッドを使用する方法があります。このメソッドは、0から1未満の範囲の乱数を生成します。

```java
// 0から1未満の範囲の乱数を生成する
double randomNumber = Math.random();
System.out.println("乱数: " + randomNumber);
```

出力例:

```
乱数: 0.5432909306414467
```

さらに、シード値を指定して乱数を生成することもできます。シード値を指定することで、同じランダムな数列を繰り返し生成することができます。

```java
// シード値を指定して乱数を生成する
Random random = new Random(123);
System.out.println("乱数: " + random.nextInt(10));
System.out.println("乱数: " + random.nextInt(10));
System.out.println("乱数: " + random.nextInt(10));
```

出力例:

```
乱数: 5
乱数: 4
乱数: 9
```

## 関連リンク

- [Javaの乱数生成方法のまとめ](https://qiita.com/turmericN/items/70b8c9bfd3a3a3ec5bf9)
- [JavaのRandomクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [JavaのMathクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)