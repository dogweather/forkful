---
title:                "乱数の生成"
aliases:
- /ja/java/generating-random-numbers/
date:                  2024-01-27T20:34:41.201371-07:00
model:                 gpt-4-0125-preview
simple_title:         "乱数の生成"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

乱数の生成とは、定義された範囲内で予測不可能な一連の数値または単一の値を生成することについてです。プログラマーは、シミュレーション、ゲーム、セキュリティアプリケーション、さまざまな条件下でのアルゴリズムのテストを目的としたサンプリング方法など、さまざまな理由からこの技術を使用します。

## どのようにして？

Javaでは、`java.util`パッケージの`Random`クラス、または特定の使用例のための`ThreadLocalRandom`および`SecureRandom`クラスを使用して乱数を生成することができます。次の例は、これらのクラスの使用方法を示しています。

### `Random`クラスの使用
`Random`クラスは、単純な擬似乱数を生成する方法を提供します。

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // Randomオブジェクトを作成

        int randInt = rand.nextInt(50); // 0から49までのランダムな整数を生成
        double randDouble = rand.nextDouble(); // 0.0から1.0の間のランダムなdoubleを生成
        boolean randBoolean = rand.nextBoolean(); // ランダムなbooleanを生成
        
        System.out.println("Random Int: " + randInt);
        System.out.println("Random Double: " + randDouble);
        System.out.println("Random Boolean: " + randBoolean);
    }
}
```

### `ThreadLocalRandom`クラスの使用
同時実行アプリケーションでは、`ThreadLocalRandom`は`Random`よりも効率的です。

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // 1から100まで
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // 1.0から10.0まで
        
        System.out.println("Random Int: " + randInt);
        System.out.println("Random Double: " + randDouble);
    }
}
```

### `SecureRandom`クラスの使用
暗号化操作のために、`SecureRandom`はより高いレベルのセキュリティを提供します。

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // セキュアなランダム数値でbytesを満たす
        
        System.out.println("Secure Random Bytes:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## 深掘り

乱数生成は、コンピューティングの初期の日々から大きく進化しています。Javaの`Random`クラスは、線形合同式を使用して擬似乱数を生成しており、これは決定論的であり高セキュリティのアプリケーションには不向きです。これが、より洗練されたアルゴリズム（例：SHA1PRNG）を使用して暗号学的に強力な乱数を生成する`SecureRandom`の導入につながりました。

しかし、`Random`および`SecureRandom`には、マルチスレッド環境での性能の低下などの短所があります。この問題を解決するために、Java 7では`ThreadLocalRandom`クラスが導入され、スレッドローカルな乱数生成器を提供することで、同時実行アプリケーションの性能を大幅に向上させました。

これらのクラスがほとんどのニーズを満たしている一方で、極めて高度なスケールや特殊な要件がある開発者は、追加のライブラリを探求するか、カスタムソリューションを開発するかもしれません。使用例のセキュリティニーズと性能要件に基づいて正しいアプローチを選択することが重要です。
