---
title:                "ランダム数の生成"
date:                  2024-01-20T17:48:30.459750-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
ランダム数生成とは予測不可能な数を作るプロセスだ。プログラマーはゲーム、シミュレーション、セキュリティ等で必要とするためにこの技術を使う。

## How to: (やり方)
C言語でのランダム数生成例を見てみよう。まず、`stdlib.h`をインクルードし、`rand()`を使う。シード値は一般的には`time(NULL)`を`unsigned`にキャストして`unsigned`の`rand()`に容易に使用する`srand()`で設定される。

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // 初期化
    srand((unsigned int)time(NULL));

    // ランダムな数の生成
    int random_number = rand() % 50;  // 0-49までの数
    printf("Random Number: %d\n", random_number);

    return 0;
}
```

サンプル出力:
```
Random Number: 23
```

## Deep Dive (掘り下げ)
最初の`rand()`は1969年のUnixの一部として出てきた。それ以来、様々な方法が開発された。`rand()`は簡単だが、予測可能なのでセキュリティが必要な場面では避けるべきだ。代わりに、POSIXで定義されているより強力な`random()`や`/dev/random`、`/dev/urandom`をLinuxで使う方法、または最新のC標準であるC11に登場した`<stdalign.h>`内の`alignof`と`<stdnoreturn.h>`内の`noreturn`のような新機能もある。

## See Also (関連情報)
- C11標準書: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
- Linux manページ (`random`について): https://linux.die.net/man/3/random
- 疑似乱数と暗号学的に安全な乱数の違い: https://www.cse.wustl.edu/~jain/cse567-06/ftp/k_26rng.pdf
