---
title:                "１つの文字列を補間する"
html_title:           "C: １つの文字列を補間する"
simple_title:         "１つの文字列を補間する"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

**## なに&なぜ？**
スプリントを使うことの意味は、プログラマーが文字列の一部を別の値で置き換えることです。文字列内の変数を動的に変化させることができるため、より効率的なコードを作成することができます。

**## 使いかた:**
以下のコード例を参考にして、スプリントを実装する方法を学んでみましょう。

```C
#include <stdio.h>
int main(){
    char name[20] = "太郎";
    int age = 23;
    float weight = 58.5;
    printf("こんにちは、私の名前は%sです。年齢は%d歳で、体重は%.2fkgです。", name, age, weight);
    return 0;
}
```

**## 詳細を探る:**
スプリントは、C言語の標準出力関数であるprintfをより柔軟に使うことができるようにするために開発されました。代替手段としては、文字列連結を行うことが挙げられますが、スプリントを使うことでかしこく簡潔なコードを実現することができます。スプリントを実装するには、変数を使用して文字列の中に挿入することができる特別な文字列フォーマットを使用します。また、スプリントの使用は動的なプログラム開発においても非常に重要です。

**## 関連リンク:**
- [C 言語公式ウェブサイト](https://ja.wikipedia.org/wiki/C%E8%A8%80%E8%AA%9E)
- [C 言語ドキュメント](https://ja.cppreference.com/w/c)
- [C 言語プログラミング入門](https://www.javadrive.jp/c/index1.html)