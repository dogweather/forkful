---
title:    "C++: デバッグ出力の印刷"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

## なぜ

デバッグ出力をプリントすることにとってと、プログラマーにとって非常に便利なものです。デバッグ出力を利用することで、コードの実行中に起きたエラーやバグの原因を特定し、修正することができます。

## How To

## 方法

プログラミングでデバッグ出力をプリントする方法を説明します。まずは、`#include <iostream>`を使って`iostream`ライブラリを読み込みます。次に、出力したい内容を`cout`を使ってプリントします。例えば、変数の値を確認したい場合は、`cout << "変数名: " << 変数 << endl;`というように書きます。最後に、`return`文を使ってプログラムを正常に終了させます。

```C++
#include <iostream>

using namespace std;

int main() {
    int number = 10;
    cout << "変数の値: " << number << endl;

    return 0;
}
```

実行すると、以下のように出力されます。

```
変数の値: 10
```

## Deep Dive

## ディープダイブ

デバッグ出力を行う際には、プログラムの実行中に細かい情報を出力することで、より詳細なデバッグを行うことが可能です。例えば、ループ処理の中で、各ステップでの変数の値を出力することで、どの部分でバグが発生しているかを特定することができます。

また、出力する情報をカスタマイズすることもできます。例えば、出力する文字列の色を変えることで、見やすくデバッグすることができます。

## See Also

## 関連リンク

- [Visual Studioでデバッグ出力を行う方法](https://docs.microsoft.com/ja-jp/visualstudio/debugger/using-the-output-window?view=vs-2019)
- [デバッグ出力で活用できるトリック集](https://www.seastea.net/entry/2018/12/01/135759)
- [printfデバッグを便利にするTips](https://qiita.com/cti/items/cd2d7a815764f2d4e9da)