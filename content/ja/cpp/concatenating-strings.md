---
title:                "「文字列の連結」"
html_title:           "C++: 「文字列の連結」"
simple_title:         "「文字列の連結」"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
文字列の連結を行う理由を2文以内で説明します。

文字列を結合することで、より複雑なデータを作成し、プログラムの柔軟性を高めることができます。例えば、複数の文字列を結合してメッセージを作成したり、ファイルパスを作成したりすることができます。

## 方法
```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  // 2つの文字列を宣言
  string name = "太郎";
  string message = "こんにちは、";

  // 文字列を連結して新しいメッセージを作成
  string greeting = message + name;

  // 結果を出力
  cout << greeting << endl;

  return 0;
}
```

出力結果:
```
こんにちは、太郎
```

## ディープダイブ
文字列の連結には、C++では`+`演算子を使用します。これは、左側の文字列に右側の文字列を結合することを意味します。また、`+=`演算子を使用することで、既存の文字列に新しい文字列を追加することができます。

文字列を結合する場合、文字列の長さやコンピュータの処理能力によって、パフォーマンスに影響が出ることがあります。そのため、大量の文字列を多数回結合する場合は、効率的な方法を検討する必要があります。

See Also:
- http://www.cplusplus.com/reference/string/string/operator+/
- http://www.cplusplus.com/reference/string/string/operator+=/