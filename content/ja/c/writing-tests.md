---
title:                "テストの書き方"
html_title:           "C: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

テストを書くことで、何度も同じコードを手動で実行する必要がなくなります。これにより、バグを見つけるのに費やす時間が短縮されるだけでなく、コードの安全性を確保することもできます。

## テストを書く方法

まず、[C言語の最新バージョン](https://ja.wikipedia.org/wiki/C言語)をダウンロードし、インストールします。そして、[Unity](https://github.com/ThrowTheSwitch/Unity)というテストフレームワークを使用することで、より簡単にテストを書くことができます。

以下は、簡単な足し算の関数をテストする例です。まず、```calculator.h```というヘッダーファイルを作成し、以下のコードを記述します。

```C
int add(int a, int b);
```

次に、```calculator.c```というソースファイルを作成し、以下のコードを記述します。

```C
#include "calculator.h"

int add(int a, int b)
{
    return a + b;
}
```

最後に、```test_calculator.c```というテストファイルを作成し、以下のコードを記述します。

```C
#include "calculator.h"
#include "unity.h"

void test_add(void)
{
    TEST_ASSERT_EQUAL_INT(2, add(1, 1));
    TEST_ASSERT_EQUAL_INT(0, add(-1, 1));
    TEST_ASSERT_EQUAL_INT(-2, add(-1, -1));
}

int main(void)
{
    UNITY_BEGIN();
    RUN_TEST(test_add);
    return UNITY_END();
}
```

このテストでは、```TEST_ASSERT_EQUAL_INT(expected, actual)```というマクロを使用して、各ケースの結果が期待値と同じであることを確認しています。

最後に、ターミナルで以下のコマンドを実行し、テストを実行してみましょう。

```
gcc calculator.c test_calculator.c -o test_calculator.out
./test_calculator.out
```

すると、以下のような出力が得られます。

```
[-----------------UNITY TESTING BEGIN----------------]
test_calculator.c:10:test_add:PASS
test_calculator.c:11:test_add:PASS
test_calculator.c:12:test_add:PASS
[------------------UNITY TESTING END-----------------]

---------------Test case count:3----------------------
--------------------Pass count:3---------------------
--------------------Fail count:0---------------------
```

テストの結果が全てパスしていることが確認できますね。

## テストを書く際の詳細

テストを書く際には、どの程度のカバレッジが必要なのか、どのようにテストを実行するのか、どのようなテストツールを使用するのかなど、さまざまな考慮事項があります。また、テストの階層構造や、モックを使用する方法など、より詳細なテスト手法も存在します。

しかし、基本的には「初めてテストを書く場合でも、簡単なテストから始めてみること」が大切です。その後、より高度なテスト手法やツールを学ぶことで、より品質の高いコードを作ることができるようになります。

## See Also

- [C言語の最新バージョンをダウンロードする方法](https://docs.oracle.com/en/database/oracle/oracle-database/21/lnpls/download.html)
- [テストフレームワークUnityの使い方](https://github.com/ThrowTheSwitch/Unity/blob/master/docs/UnityGettingStartedGuide.md)
-