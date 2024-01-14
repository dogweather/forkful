---
title:                "C: テストを書く"
simple_title:         "テストを書く"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

プログラミングは、エラーが起きない完璧なコードを書くことは不可能です。しかし、プログラムにバグがあると、予期せぬ結果を引き起こす可能性があり、時には致命的な結果になることもあります。こうした意外な問題を防ぐため、テストを書くことが非常に重要です。

## テストの書き方

テストを書くには、C言語のアサート（assert）を使用します。これは、プログラム実行時に特定の条件が満たされていることを確認するものです。例えば、関数の戻り値が正しいかどうかをテストすることができます。

```C
#include <stdio.h>
#include <assert.h>

// テストする関数
int square(int num){
    return num * num;
}

int main(){
    // アサートを使用してテストを実行
    assert(square(2) == 4);
    assert(square(5) == 25);
    assert(square(-3) == 9);
    printf("テストは成功しました！\n");
    return 0;
}
```

上記のコードでは、アサートを使用してテストを行っています。もし、条件を満たさない場合はプログラムが停止してエラーを出力します。これにより、プログラム実行中に問題が発生したことがわかり、バグを修正することができます。

## テストの詳細

テストを書く際には、どのような条件をテストするかを良く考えることが大切です。また、プログラムの各部分を個別にテストすることも重要です。これにより、問題が起きた時に原因を特定しやすくなります。

さらに、テストを自動化することも重要です。手動でテストを行うと、時間がかかり正確性も低くなります。そのため、自動化されたテストを使うことで効率的にバグを見つけることができます。

## See Also

- [C言語のテスト駆動開発](https://www.ibm.com/developerworks/jp/linux/library/l-lpic1-103-4/)
- [JUnitでのテスト自動化の基本](https://www.ibm.com/developerworks/jp/java/library/j-junitbasics.html)
- [シアトル大学：ソフトウェア開発の創造性と力](https://www.coursera.org/specializations/dukeklobucharspecialization/1)