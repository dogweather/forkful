---
title:                "C++: 「一時ファイルの作成」"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# なぜ
テンポラリファイルを作成することに携わる理由について、本記事では紹介します。

## 作成方法
以下のようなステップでC++プログラミングを使用して、テンポラリファイルを作成することができます。

```C++
#include <iostream>
#include <fstream>

// ファイル名を指定して一時ファイルを作成する
std::ofstream tempFile("temporary.txt");

// ファイルに内容を書き込む
tempFile << "これはテストファイルです。" << std::endl;

// ファイルを閉じる
tempFile.close();
```

上記のコードを実行すると、`temporary.txt`という名前の一時ファイルが作成されて、指定した内容が書き込まれます。

## 深堀り
テンポラリファイルを作成することによって、アプリケーションが一時的に必要とするデータを保存することができます。また、ファイルのパスを指定せずに作成することができるため、エラーが起きる可能性が減ります。

また、一時ファイルには自動的に削除されるタイムスタンプが付与されるため、プログラムの実行後に手動でファイルを削除する必要がありません。

# 参考リンク
- [std::ofstream - C++ Reference](https://www.cplusplus.com/reference/fstream/ofstream/)
- [C++で一時ファイルを扱う](https://qiita.com/walkers/items/9b1070820e255c9b380a)