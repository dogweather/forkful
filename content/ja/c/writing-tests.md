---
title:                "C: テストの書き方"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-tests.md"
---

{{< edit_this_page >}}

今回は、テストコードを書くことの重要性についてお伝えします。プログラムの品質を保証する上で、テストは欠かせないものとなっています。では、テストコードをどのように書くかを見ていきましょう。

## Why
テストコードを書く主な目的は、自分のコードや他の人のコードが正しく動作するかどうかを確かめることです。コードにバグがあると、それがプログラム全体に大きな影響を与える可能性があります。しかし、テストコードを書くことで、バグを発見し修正することができます。さらに、コードの変更や追加を行った際にも、テストコードがあれば正しく動作していることを確認することができます。

## How To
テストコードを書くにあたっては、一般的には次のようなプロセスになります。

1. テスト対象のコードを読んで、どのような場合にどのような結果が得られるかを把握する
2. テストケースを作成し、期待される結果を明確にする
3. コードを実行し、テストが正しく動作するかを確認する
4. バグが見つかった場合は、その原因を特定し修正する

以下は、簡単な関数をテストする例です。

```C
#include <stdio.h>

/* 値を2倍にする関数 */
int double_value(int num) {
    return num * 2;
}

int main() {
    int result = double_value(5);
    printf("Result: %d\n", result);
    return 0;
}
```

この関数をテストするためには、以下のようなテストコードを書きます。

```C
#include <stdio.h>

/* テスト対象の関数 */
int double_value(int num) {
    return num * 2;
}

int main() {
    int expected_result = 10;
    int actual_result = double_value(5);

    /* テストを実行し、結果を確認する */
    if (actual_result == expected_result) {
        printf("Test passed!\n");
    } else {
        printf("Test failed. Expected: %d, Actual: %d\n", expected_result, actual_result);
    }

    return 0;
}
```

上記のテストコードを実行すると、`Test passed!`というメッセージが表示されるはずです。このように、テストコードを書くことで、コードの動作を確認することができます。

## Deep Dive
テストコードを書く際には、いくつかのポイントに注意することが重要です。例えば、テストケースを作成する際には、コーナーケースやエッジケースに対してもテストすることが必要です。また、テストコードのメンテナンスも重要であり、新しい機能が追加された際には、それに応じてテストコードも更新する必要があります。

テストコードはプログラムの品質を向上させる上で非常に重要なものです。プログラマーとして、テストコードをしっかりと書くことを意識しましょう。

## See Also
- [プログラムテストの基本](https://www.ipa.go.jp/sec/softwaretest/test_guide5.html)
- [単体テストの書き方](https://www.qa-check.com/test_types/test_case/unit?page=1)