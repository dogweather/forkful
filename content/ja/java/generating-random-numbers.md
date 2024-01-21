---
title:                "ランダム数の生成"
date:                  2024-01-20T17:49:35.444286-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
乱数生成とは、予測不可能な数字のシーケンスを作るプロセスです。プログラマーは、テストデータ生成、シミュレーション、ゲームの要素など、さまざまな状況でこれを利用します。

## How to: (どのように？)
Javaでは、`Random`クラスや`SecureRandom`、Java 8で導入された`ThreadLocalRandom`、そして`Random`クラス以上に推奨される`java.util.concurrent`パッケージの`ThreadLocalRandom`を使って乱数を生成します。サンプルコードを見てみましょう。

```java
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

public class RandomNumbers {
    public static void main(String[] args) {
        // Random クラスを使う
        Random random = new Random();
        int randomInt = random.nextInt(100); // 0 から 99 までの乱数
        System.out.println("Random int: " + randomInt);
        
        // ThreadLocalRandom クラスを使う
        int tlRandomInt = ThreadLocalRandom.current().nextInt(100);
        System.out.println("ThreadLocalRandom int: " + tlRandomInt);
    }
}
```

サンプル出力：
```
Random int: 42
ThreadLocalRandom int: 85
```

## Deep Dive (深い潜入)
乱数生成は、計算機の歴史と共に進化し続けています。初期のコンピュータでは、物理プロセスに基づいたハードウェア乱数ジェネレータが使われていました。ソフトウェアベースでは、線形合同法(LCG)など古典的なアルゴリズムが広く使われてきました。

`Random` クラスは内部で線形合同法を使用していますが、高性能・高並列性を求めるアプリケーショのニーズに応えるため`java.util.concurrent`パッケージの `ThreadLocalRandom` が推奨されます。`ThreadLocalRandom` は、乱数生成を複数のスレッドで効率よく行えるようにするために作られています。

セキュリティが重要な場面では、`SecureRandom` クラスが確率論的ランダム性を提供し、より予測が困難な乱数を生成します。これは暗号ソリューションで一般的に使用されます。

## See Also (関連項目)
- Oracle Docs – Random Class: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Random.html
- Oracle Docs – ThreadLocalRandom Class: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/concurrent/ThreadLocalRandom.html
- Oracle Docs – SecureRandom Class: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/security/SecureRandom.html