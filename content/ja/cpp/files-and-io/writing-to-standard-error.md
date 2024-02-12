---
title:                "標準エラーへの書き込み"
aliases: - /ja/cpp/writing-to-standard-error.md
date:                  2024-02-03T19:32:54.380592-07:00
model:                 gpt-4-0125-preview
simple_title:         "標準エラーへの書き込み"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
