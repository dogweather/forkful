---
title:                "エラー処理"
aliases:
- ja/c/handling-errors.md
date:                  2024-02-03T17:58:27.350751-07:00
model:                 gpt-4-0125-preview
simple_title:         "エラー処理"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Cでのエラー処理は、プログラム実行中に発生する異常な状況を検出して対応することを含みます。プログラマーはこれを行うことで、バグ、クラッシュ、そして予測不可能な振る舞いを防ぎ、さまざまなシナリオの下でソフトウェアが信頼性と効率的に機能することを保証します。

## 方法：

Cは他の言語のように例外のための組み込みサポートを持っていません。代わりに、関数から特別な値を返す、`errno`のようなグローバル変数を設定するなど、いくつかの従来のエラー処理戦略に頼っています。

**特別な値を返す**

関数は、有効な結果としてありそうにない特定の値を返すことでエラーを示すことができます。以下は整数の例です：

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // エラーケース
    } else {
        *result = 1.0 / number;
        return 0; // 成功
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("Error: Division by zero.\n");
    } else {
        printf("The inverse is: %f\n", result);
    }
    
    return 0;
}
```

**出力：**
```
Error: Division by zero.
```

**`errno`をチェックする**

システムやOS（ファイルI/Oなど）とやり取りする特にライブラリ関数では、エラーが発生すると`errno`が設定されます。これを使用するには、`errno.h`をインクルードして、予想される失敗の後で`errno`をチェックします：

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("Error opening file: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**出力：**
```
Error opening file: No such file or directory
```

## ディープダイブ

歴史的に、Cプログラミング言語のミニマリスティックなデザインは、その低レベルでシステムプログラミングの起源を反映し、最大の性能とメタルに近い制御が重要な場合に、組み込みの例外処理メカニズムを除外してきました。代わりに、Cはプログラマーに可能な限り多くの制御を提供するというその哲学に適合する、より手動のエラー処理アプローチを採用しています。これは便利さのコストであってもです。

このアプローチはCの設計目標とよく一致しているものの、冗長なエラーチェックコードや見逃されたエラーチェックの可能性を生じさせ、近代的な言語が構造化された例外処理メカニズムで対処しているものです。例えば、JavaやC#のような言語での例外は、エラー処理を中央化し、コードをクリーナーにし、エラー管理をより直接的にすることができます。しかし、例外はそれ自体のオーバーヘッドや複雑さを導入し、Cが輝くシステムレベルのプログラミングには理想的ではないかもしれません。

その粗野さにもかかわらず、Cにおけるこの手動のエラー処理は、エラー条件の明確さがより予測可能でデバッグしやすいコードにつながるモデルを多くの他の言語のエラー管理の設計に情報を提供しています。致命的なシステムで、障害を優雅に管理する必要がある場合、Cのエラー処理パラダイムは、エラー処理ライブラリや規約などの現代のベストプラクティスと組み合わせることで、堅牢性と信頼性を保証します。
