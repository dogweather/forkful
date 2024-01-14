---
title:                "C++: 「文字列を小文字に変換する」"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

このプログラミングブログでは、C++での文字列を小文字に変換する方法について説明します。文字列を小文字に変換する理由は、通常は比較や処理のために使用されます。例えば、ユーザーの入力を正しく処理するために、文字列の大文字と小文字を区別せずに処理する必要がある場合があります。

## 方法

文字列を小文字に変換する方法には、いくつかの異なるアプローチがあります。ここでは、基本的な方法をいくつかの例と共に紹介します。

```C++
#include <iostream>
#include <string>
#include <algorithm> // transform関数を使用するために必要

using namespace std;

int main(){
    string str = "Hello World!";
    transform(str.begin(), str.end(), str.begin(), ::tolower); // 文字列を小文字に変換

    cout << str << endl; // 出力結果: hello world!
    return 0;
}
```

もうひとつの方法は、ASCIIコードを使用する方法です。英字の大文字と小文字の間には、10進数の差が32あることを利用して、文字列を小文字に変換することができます。以下の例では、ループを使用して文字列の各文字を小文字に変換しています。

```C++
#include <iostream>
#include <string>

using namespace std;

int main(){
    string str = "Hello World!";
    
    for (int i = 0; i < str.length(); i++) {
        if (str[i] >= 65 && str[i] <= 90) {
            str[i] += 32; // ASCIIコードの差を利用して小文字に変換
        }
    }

    cout << str << endl; // 出力結果: hello world!
    return 0;
}
```

## 深く潜る

文字列を小文字に変換する方法について、さらに詳しく見ていきましょう。

文字の大文字と小文字の間には、10進数の差が32あると説明しましたが、これはASCIIコードの範囲内の英字に限らず、ほとんどの場合に適用することができます。しかし、例外もあります。例えば、Unicode文字ではこの方法ではうまくいかない場合があります。

また、今回紹介した2つの方法は、全ての言語で機能するわけではありません。特定の文字セットやエンコーディングによっては、上記の方法がうまく機能しない場合もあります。

より確実な方法としては、C++の標準ライブラリである`locale`を使用する方法があります。これは地域や言語に応じた文字列操作を行うためのもので、安全な方法と言えるでしょう。

## このようにして文字列を小文字に変換する

文字列を小文字に変換する方法は多数ありますが、基本的にはASCIIコードを使用するか、`locale`を使用する方法が推奨されます。しかし、プロジェクトや環境によっては、さまざまな方法が存在する場合もあるので、まずはプロジェクトの要件や標準を確認することが重要です。

## 参考リンク

[ASCIIコード表](https://ja.wikipedia.org/wiki/ASCII)  
[C++ ドキュメント: transform関数](https://cpprefjp.github.io/reference/algorithm/transform.html)  
[C++ ドキュメント: locale](https://cpprefjp