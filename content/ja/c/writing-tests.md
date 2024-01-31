---
title:                "テストの作成"
date:                  2024-01-19
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"

category:             "C"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストコードの「何」と「理由」)
テストコードはプログラムが意図した動作をするかを確認するコードです。バグの早期発見、機能の正確性を保証し、安心してリファクタリングできるようにするために書きます。

## How to: (やり方)
以下の例では、単純な関数のテストコードを示します:

```C
#include <assert.h>

// 関数の宣言
int add(int a, int b) {
    return a + b;
}

// テスト関数
void test_add() {
    assert(add(2, 3) == 5);
    assert(add(-1, 1) == 0);
    assert(add(0, 0) == 0);
}

// main関数でテストを実行
int main() {
    test_add();
    return 0; // すべてのテストが成功すると、プログラムは0を返す
}
```

期待される出力はなし（アサーションに失敗するとプログラムは停止します）。

## Deep Dive (深掘り)
テストコードは1970年代に「ボックステスト」として発展しました。代替手段には手動テスト、統合テスト、受け入れテストがあります。C言語では`assert.h`ライブラリを使用する単体テストが一般的ですが、複雑なテストには`CUnit`や`Check`などのテストフレームワークを使用します。

## See Also (関連情報)
- CUnit: http://cunit.sourceforge.net/
- Check: https://libcheck.github.io/check/
- Cプログラムのテストについての詳細な解説: "Cプログラムのテスト戦略" by Brian Kernighan and Rob Pike
