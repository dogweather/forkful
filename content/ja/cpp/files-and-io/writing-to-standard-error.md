---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:54.380592-07:00
description: "C++\u3067\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08`stderr`\uFF09\u306B\u66F8\
  \u304D\u8FBC\u3080\u3053\u3068\u306F\u3001\u30E1\u30A4\u30F3\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u306E\u51FA\u529B\u3068\u306F\u5225\u306E\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\
  \u30FC\u30B8\u3084\u8A3A\u65AD\u60C5\u5831\u3092\u51FA\u529B\u3059\u308B\u3053\u3068\
  \u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u30A8\u30E9\u30FC\u3092\u7570\u306A\
  \u308B\u30B9\u30C8\u30EA\u30FC\u30E0\u306B\u5411\u3051\u3001\u901A\u5E38\u306E\u51FA\
  \u529B\u3068\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u533A\u5225\u3059\
  \u308B\u3053\u3068\u306B\u3088\u308A\u3001\u30C7\u30D0\u30C3\u30B0\u3068\u30A8\u30E9\
  \u30FC\u51E6\u7406\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:16.125220-06:00'
model: gpt-4-0125-preview
summary: "C++\u3067\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08`stderr`\uFF09\u306B\u66F8\u304D\
  \u8FBC\u3080\u3053\u3068\u306F\u3001\u30E1\u30A4\u30F3\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u306E\u51FA\u529B\u3068\u306F\u5225\u306E\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\
  \u30B8\u3084\u8A3A\u65AD\u60C5\u5831\u3092\u51FA\u529B\u3059\u308B\u3053\u3068\u3092\
  \u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\
  \u308C\u3092\u884C\u3046\u3053\u3068\u3067\u30A8\u30E9\u30FC\u3092\u7570\u306A\u308B\
  \u30B9\u30C8\u30EA\u30FC\u30E0\u306B\u5411\u3051\u3001\u901A\u5E38\u306E\u51FA\u529B\
  \u3068\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u533A\u5225\u3059\u308B\
  \u3053\u3068\u306B\u3088\u308A\u3001\u30C7\u30D0\u30C3\u30B0\u3068\u30A8\u30E9\u30FC\
  \u51E6\u7406\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
---

{{< edit_this_page >}}

## 何となぜ？

C++で標準エラー（`stderr`）に書き込むことは、メインプログラムの出力とは別のエラーメッセージや診断情報を出力することを意味します。プログラマーはこれを行うことでエラーを異なるストリームに向け、通常の出力とエラーメッセージを区別することにより、デバッグとエラー処理を容易にします。

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
