---
title:    "C: テストの書き方"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書く必要があるのか
テストを書くことは、プログラミングにおいて非常に重要です。テストを書くことにより、バグやエラーを事前に発見することができ、コードの品質を向上させることができます。

## テストの書き方
テストを書くためには、まずテストフレームワークを選ぶ必要があります。テストフレームワークには様々な種類がありますが、この記事ではC言語で使用できるUnityというフレームワークを例として紹介します。

```
#include <stdio.h>
#include "unity.h"

/* テストしたい関数の宣言 */
int add(int x, int y);

/* テストケースの記述 */
void test_add(void) {
    TEST_ASSERT_EQUAL(5, add(2, 3));
    TEST_ASSERT_EQUAL(-1, add(0, -1));
    TEST_ASSERT_EQUAL(100, add(50, 50));
}

/* メイン関数 */
int main(void) {
    /* テストランナーの初期化 */
    UNITY_BEGIN();

    /* テストケースの実行 */
    RUN_TEST(test_add);

    /* テストランナーの終了 */
    return UNITY_END();
}

/* テストする関数の実装 */
int add(int x, int y) {
    return x + y;
}
```

上記のコードは、add関数をテストする際の例です。UNITY_BEGIN()とUNITY_END()で囲まれた部分がテストランナーの初期化と終了を行う部分で、RUN_TEST()でテストケースを実行することができます。また、TEST_ASSERT_EQUAL()ではテストしたい値と実際の値が等しいかどうかをチェックすることができます。

## テストについての詳細
テストを書く際、重要なのは網羅性と信頼性です。網羅性とは、できるだけ多くのケースをテストすることであり、信頼性とはテストが正しく動作することを確認することです。

また、テストを書くことで、新しい機能を追加した際に既存の機能に影響を与えないかどうかも確認することができます。さらに、テストコードを書くことでチーム全体でコードの品質を保つことができます。

## 他の参考記事
[テストの重要性についての詳細](https://www.tdi.co.jp/miso/gihou/eshi/0703/index.html)

[Unity公式サイト](https://unity.dochub.io/)

[テスト駆動開発（TDD）とは何か](https://www.tdi.co.jp/miso/gihou/eshi/1509/index.html)

## 参考文献
- [テストドリブン開発入門～テスト駆動開発（TDD）の基礎と考え方～ - サムライエンジニアブログ](https://www.tdi.co.jp/miso/gihou/eshi/0703/index.html)
- [テストフレームワークUnity公式サイト](https://unity.dochub.io/)
- [テスト駆動開発（TDD）とは何か - サムライエンジニアブログ](https://www.tdi.co.jp/miso/gihou/eshi/1509/index.html)