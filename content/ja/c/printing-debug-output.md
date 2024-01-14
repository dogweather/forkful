---
title:    "C: デバッグ出力のプリント"
keywords: ["C"]
---

{{< edit_this_page >}}

## なぜ

プログラムのデバッグを行う際、デバッグ出力を行うことはとても重要です。デバッグ出力を行うことで、プログラムの実行中に発生するエラーや問題を素早く特定し、修正することができます。

## 方法

デバッグ出力を行うには、標準出力関数である`printf()`を使用します。以下の例では、変数`num`の値を表示します。

```C
#include <stdio.h>

int main()
{
    int num = 5;
    printf("numの値は%dです。", num);
    return 0;
}

// Output:
// numの値は5です。
```

また、デバッグ出力を行う際には、`printf()`を条件文やループ文の中に入れることで、特定の場所でのみ出力することもできます。

```C
#include <stdio.h>

int main()
{
    int num = 5;
    for (int i = 0; i < 10; i++)
    {
        if (i == num)
        {
            printf("numの値は%dです。", num);
        }
    }
    return 0;
}

// Output:
// numの値は5です。
```

## ディープダイブ

デバッグ出力を行う際には、出力したい情報とそのフォーマットについて考えることが重要です。出力される情報が分かりやすいように、メッセージには変数の値や結果を含めることができます。また、整形指定子を使用することで、文字列や変数を特定の形式で表示することもできます。

例えば、小数点以下2桁までの浮動小数点数を表示する際には、`%.2f`という指定子を使用できます。

```C
#include <stdio.h>

int main()
{
    double num = 3.14159;
    printf("円周率は%.2fです。", num);
    return 0;
}

// Output:
// 円周率は3.14です。
```

さらに、複数の変数を出力する際には、カンマで区切ることで順番に値が表示されます。

```C
#include <stdio.h>

int main()
{
    int num1 = 10;
    int num2 = 20;
    printf("num1の値は%dで、num2の値は%dです。", num1, num2);
    return 0;
}

// Output:
// num1の値は10で、num2の値は20です。
```

デバッグ出力においては、このように出力のフォーマットを工夫することで、問題の特定や修正がよりスムーズに行えるようになります。

## 関連リンク

[C言語入門 - デバッグとprintf](https://www.javadrive.jp/cstart/printf/index7.html)

[C - printf()関数について初心者向けに解説【基本編】](https://www.sejuku.net/blog/16924)

[C言語のデバッグの方法～printfデバッグを使う～](https://qiita.com/moguno/items/0f5abf53d630f468c615)

[Vimプラグインで幸せになるためのデバッグ Printならprintf.vim](https://qiita.com/prince_nano/items/aa565c088c0cbb301f13)

```japanese
# また、いつもお使いのテキストエディタには、デバッグ出力を行うためのプラグインがあるかもしれませんので、お調べください。