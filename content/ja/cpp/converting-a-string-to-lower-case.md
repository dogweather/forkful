---
title:    "C++: 「文字列を小文字に変換する」"
keywords: ["C++"]
---

{{< edit_this_page >}}

## なぜ

C ++で文字列を小文字に変換するのには、様々な理由があります。例えば、ユーザーが入力した文字列を統一された形式で処理する場合や、文字列の比較を行う際に大文字と小文字を区別したくない場合があります。

## 方法

C ++で文字列を小文字に変換するには、標準ライブラリの`std::tolower`を使用します。以下の例では、`std::transform`を使って文字列全体を小文字に変換し、結果を新しい変数に保存しています。

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string str = "Hello World!";
    std::string lowercase_str;

    std::transform(str.begin(), str.end(), lowercase_str.begin(), ::tolower);

    std::cout << lowercase_str << std::endl; // 出力：hello world!

    return 0;
}
```

`std::transform`は、与えられた範囲の全ての要素に対して処理を行うため、文字列内の全ての文字を小文字に変換することができます。

## 詳細を掘り下げる

C ++では、文字列を小文字に変換する際にも様々なオプションがあります。`std::tolower`を使う方法の他にも、`<cctype>`ヘッダーファイルに含まれる関数を組み合わせる方法や、自分で記述するカスタム関数を使用する方法などがあります。

また、言語や環境によっては、大文字と小文字を区別しないで比較を行う機能が搭載されている場合もあります。文字列を小文字に変換することで、このような機能を活用することができます。

## 関連リンク

- [C++ Reference: std::tolower](https://en.cppreference.com/w/cpp/string/byte/tolower)
- [C++ Reference: std::transform](https://en.cppreference.com/w/cpp/algorithm/transform)
- [C++日本語リファレンス](https://cpprefjp.github.io/reference/)