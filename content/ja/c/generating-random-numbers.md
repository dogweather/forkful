---
title:                "乱数の生成"
date:                  2024-01-27T20:33:15.489575-07:00
model:                 gpt-4-0125-preview
simple_title:         "乱数の生成"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

Cで乱数を生成するということは、無作為の概念を模倣して、識別可能なパターンを持たない数のシーケンスを作成することです。プログラマーは、データのシミュレーション、暗号化アプリケーション、ゲーム開発など、多岐にわたる目的で乱数を活用し、それがプログラミングの重要な側面となっています。

## 方法：

Cで乱数を生成するには、通常`stdlib.h`にある`rand()`関数を使用します。しかし、異なるプログラムの実行ごとに生成される数値の変動を保証するために、乱数生成器をシード（初期化値）することが重要です。通常、現在時刻などの値でシードされた`srand()`関数がこれを実現します。

0から99の間の乱数を生成する簡単な例を以下に示します：

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // 乱数生成器をシードする
    srand((unsigned) time(NULL));

    // 0から99の間で乱数を生成する
    int randomNumber = rand() % 100;

    // 乱数を表示する
    printf("Random Number: %d\n", randomNumber);

    return 0;
}
```

サンプル出力：

```
Random Number: 42
```

このプログラムの各実行が現在時刻でシードすることによって、新しい乱数を生成する点に注意が必要です。

## 掘り下げ

Cで乱数を生成する従来の方法、すなわち`rand()`と`srand()`を使う方法は、本当に無作為ではありません。これは擬似乱数です。これは多くのアプリケーションで問題ありませんが、真剣な暗号化用途など、高いレベルの無作為性を要求する状況では不十分です。`rand()`によって生成されたシーケンスは、`srand()`に提供されたシードによって完全に決定されます。したがって、シードがわかれば、シーケンスは予測でき、無作為性が減少します。

歴史的に、`rand()`関数はその低品質な無作為性と限られた範囲に対して批判されてきました。現代の代替案には、真の無作為性をより良く近似するデバイス固有のAPIや外部ライブラリの使用、またUNIX系システムでは、暗号化目的で`/dev/random`や`/dev/urandom`から読み取る方法があります。

例えば、Cで`/dev/urandom`を使用する場合：

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // 読み取り用に/dev/urandomを開く
    fp = fopen("/dev/urandom", "r");

    // 乱数を読み取る
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // 乱数を表示する
    printf("Random Number: %u\n", randomNumber);

    // ファイルを閉じる
    fclose(fp);

    return 0;
}
```

この方法では、システムのエントロピープールから直接読み取り、より感度の高いアプリケーションに適した品質の高い無作為性を提供します。しかし、このアプローチは異なるプラットフォーム間での移植性の問題が生じる可能性があり、`rand()`を使用する方法ほど普遍的ではありません。

方法に関わらず、Cにおける無作為性の本質とその実装を理解することは、効果的で安全な、そして魅力的なアプリケーションを開発する上で重要です。
