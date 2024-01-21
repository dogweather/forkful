---
title:                "文字列の長さを求める"
date:                  2024-01-20T17:47:12.050517-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
文字列の長さを知ることは、単純にその文字列にどれだけの文字が含まれているかを数えることです。プログラマは、データ処理やメモリ管理を適切に行うために、この情報を使用します。

## How to: (方法)
```C
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "こんにちは";
    size_t length = strlen(myString);

    // Note: This length might not reflect the number of characters
    // in languages such as Japanese due to multi-byte characters.
    printf("Length: %zu\n", length);
    return 0;
}
```

実行結果:
```
Length: 15
```

## Deep Dive (深掘り)
C言語における`strlen`関数は、C標準ライブラリである`string.h`に定義されています。この関数はヌル終端文字列（文字の末尾に`\0`がある）の長さを計算します。`strlen`は、C89標準で導入され、現代のCのほぼすべての実装に存在しています。

マルチバイト文字を持つ言語、例えば日本語では、`strlen`は意図した結果を返さないこともあります。日本語などの多くの現代文字列はUTF-8でエンコードされ、それぞれの文字が複数のバイトを占める可能性があるからです。

代替方法として`mbstowcs`や`wcslen`などの関数があります。これらはワイド文字列を使って長さを正確に計算します。

```C
#include <stdio.h>
#include <wchar.h>
#include <locale.h>

int main() {
    setlocale(LC_CTYPE, "");
    wchar_t myWideString[] = L"こんにちは";
    size_t wideLength = wcslen(myWideString);

    printf("Wide Length: %zu\n", wideLength);
    return 0;
}
```

実行結果:
```
Wide Length: 5
```

この例では、先にロケールを設定することで、ワイド文字列の長さを適切に計算しています。また、コンパイラのサポートと正しいロケール設定が必要です。

## See Also (関連情報)
- C標準ライブラリドキュメント: [http://www.cplusplus.com/reference/cstring/strlen/](http://www.cplusplus.com/reference/cstring/strlen/)
- UTF-8とワイド文字について: [https://www.utf8everywhere.org/](https://www.utf8everywhere.org/)
- Multi-byte characters in C: [https://en.wikipedia.org/wiki/Multibyte_character_set](https://en.wikipedia.org/wiki/Multibyte_character_set)