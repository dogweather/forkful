---
date: 2024-01-27 20:34:41.201371-07:00
description: "\u4E71\u6570\u306E\u751F\u6210\u3068\u306F\u3001\u5B9A\u7FA9\u3055\u308C\
  \u305F\u7BC4\u56F2\u5185\u3067\u4E88\u6E2C\u4E0D\u53EF\u80FD\u306A\u4E00\u9023\u306E\
  \u6570\u5024\u307E\u305F\u306F\u5358\u4E00\u306E\u5024\u3092\u751F\u6210\u3059\u308B\
  \u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3001\u30B2\u30FC\
  \u30E0\u3001\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u3001\u3055\u307E\u3056\u307E\u306A\u6761\u4EF6\u4E0B\u3067\u306E\u30A2\
  \u30EB\u30B4\u30EA\u30BA\u30E0\u306E\u30C6\u30B9\u30C8\u3092\u76EE\u7684\u3068\u3057\
  \u305F\u30B5\u30F3\u30D7\u30EA\u30F3\u30B0\u65B9\u6CD5\u306A\u3069\u3001\u3055\u307E\
  \u3056\u307E\u306A\u7406\u7531\u304B\u3089\u3053\u306E\u6280\u8853\u3092\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.941209-06:00'
model: gpt-4-0125-preview
summary: "\u4E71\u6570\u306E\u751F\u6210\u3068\u306F\u3001\u5B9A\u7FA9\u3055\u308C\
  \u305F\u7BC4\u56F2\u5185\u3067\u4E88\u6E2C\u4E0D\u53EF\u80FD\u306A\u4E00\u9023\u306E\
  \u6570\u5024\u307E\u305F\u306F\u5358\u4E00\u306E\u5024\u3092\u751F\u6210\u3059\u308B\
  \u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3001\u30B2\u30FC\
  \u30E0\u3001\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u3001\u3055\u307E\u3056\u307E\u306A\u6761\u4EF6\u4E0B\u3067\u306E\u30A2\
  \u30EB\u30B4\u30EA\u30BA\u30E0\u306E\u30C6\u30B9\u30C8\u3092\u76EE\u7684\u3068\u3057\
  \u305F\u30B5\u30F3\u30D7\u30EA\u30F3\u30B0\u65B9\u6CD5\u306A\u3069\u3001\u3055\u307E\
  \u3056\u307E\u306A\u7406\u7531\u304B\u3089\u3053\u306E\u6280\u8853\u3092\u4F7F\u7528\
  \u3057\u307E\u3059\u3002."
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

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
