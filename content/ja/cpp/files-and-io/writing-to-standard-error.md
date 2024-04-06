---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:54.380592-07:00
description: "\u65B9\u6CD5\uFF1A C++\u3067\u6A19\u6E96\u30A8\u30E9\u30FC\u306B\u66F8\
  \u304D\u8FBC\u3080\u306B\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\
  \u4E00\u90E8\u3067\u3042\u308B`cerr`\u30B9\u30C8\u30EA\u30FC\u30E0\u3092\u4F7F\u7528\
  \u3067\u304D\u307E\u3059\u3002\u57FA\u672C\u7684\u306A\u4F8B\u3092\u4EE5\u4E0B\u306B\
  \u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:38:42.081727-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A C++\u3067\u6A19\u6E96\u30A8\u30E9\u30FC\u306B\u66F8\u304D\
  \u8FBC\u3080\u306B\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u4E00\
  \u90E8\u3067\u3042\u308B`cerr`\u30B9\u30C8\u30EA\u30FC\u30E0\u3092\u4F7F\u7528\u3067\
  \u304D\u307E\u3059\u3002\u57FA\u672C\u7684\u306A\u4F8B\u3092\u4EE5\u4E0B\u306B\u793A\
  \u3057\u307E\u3059\uFF1A."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 方法：
C++で標準エラーに書き込むには、標準ライブラリの一部である`cerr`ストリームを使用できます。基本的な例を以下に示します：

```cpp
#include <iostream>

int main() {
    // 標準出力への書き込み
    std::cout << "This is a normal message." << std::endl;
    
    // 標準エラーへの書き込み
    std::cerr << "This is an error message." << std::endl;
    
    return 0;
}
```

サンプル出力：
```
This is a normal message.
This is an error message.
```

この場合、両方のメッセージは通常、ターミナル上に表示されますが、シェルで別々にリダイレクトできます。たとえば、標準出力をファイルに送りながら、エラーを画面に表示させることができます。

より高度なログ記録やエラー処理には、`spdlog`や`boost.log`のようなサードパーティのライブラリを利用できます。これらのライブラリは、フォーマット、ログレベル、ファイル出力など、ログ記録のための拡張機能を提供します。

エラーメッセージを書き込むために`spdlog`を使用する方法は以下の通りです：

```cpp
#include "spdlog/spdlog.h"

int main() {
    // spdlogの初期化
    spdlog::info("This is a normal message.");
    spdlog::error("This is an error message.");
    
    return 0;
}
```

注：`spdlog`を使用する場合、プロジェクトに追加する必要があります。これは、GitHubからリポジトリをクローンするか、`vcpkg`や`conan`のようなパッケージマネージャを使用することで行えます。

エラー処理とログ記録に関して、アプリケーションの複雑さと特定のニーズに基づいて、標準ストリームを直接使用するか、`spdlog`のようなライブラリを使用するかを選択することを忘れないでください。
