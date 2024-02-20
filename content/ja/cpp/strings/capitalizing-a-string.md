---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:21.629398-07:00
description: "\u2026"
lastmod: 2024-02-19 22:05:01.641203
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
文字列の先頭文字を大文字化するとは、文字列内の各単語の最初の文字を小文字の場合は大文字に変換し、残りの文字を変更しないことを意味します。プログラマーは、出力のフォーマット、ユーザー入力、またはデータ処理でこのタスクを頻繁に実行して、テキストが表示または処理される方法に一貫性を確保します。特に、ユーザーインタフェースやデータ正規化のタスクでそうです。

## どのようにして：
C++では、標準ライブラリを使用して文字列を大文字化することができますが、サードパーティのライブラリは必要ありません。ただし、より複雑または特定の大文字化の振る舞いについては、Boostのようなライブラリが非常に役立つことがあります。以下に両方のアプローチを説明する例を示します。

### 標準C++ライブラリを使用する：

```cpp
#include <iostream>
#include <cctype> // std::tolower and std::toupper用
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // 出力: "Hello World From C++"
}
```

### Boostライブラリを使用する：

より高度な文字列操作、ロケール認識の大文字化を含む場合、Boost String Algoライブラリの使用を検討するかもしれません。

まず、プロジェクトでBoostライブラリがインストールされ、設定されていることを確認してください。次に、必要なヘッダを含め、以下に示すようにその機能を使用できます。

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // 各単語の最初の文字を大文字にする
    boost::algorithm::to_lower(capitalizedText); // 文字列を小文字にすることを保証する
    capitalizedText[0] = std::toupper(capitalizedText[0]); // 最初の文字を大文字にする

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // スペースの後に大文字にする
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // 出力: "Hello World From C++"
}
```

この場合、Boostは文字列操作のタスクをいくらか簡素化しますが、主に変換とケース変換のユーティリティを提供しているため、真の大文字化にはカスタムアプローチがまだ必要です。
