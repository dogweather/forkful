---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:10.082931-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.583980-06:00'
model: gpt-4-0125-preview
summary: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u30D5\u30A1\u30A4\
  \u30EB\u3092\u6271\u3046\u3053\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u5404\
  \u884C\u304C\u8868\u306E\u884C\u3092\u8868\u3057\u3001\u500B\u3005\u306E\u5217\u304C\
  \u30AB\u30F3\u30DE\u3067\u533A\u5207\u3089\u308C\u308B\u7C21\u5358\u306A\u30C6\u30AD\
  \u30B9\u30C8\u5F62\u5F0F\u306B\u683C\u7D0D\u3055\u308C\u305F\u30C7\u30FC\u30BF\u306E\
  \u51E6\u7406\u304A\u3088\u3073\u64CD\u4F5C\u306B\u3064\u3044\u3066\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001CSV\u304C\u8EFD\u91CF\u3067\u4EBA\
  \u304C\u8AAD\u3081\u308B\u30C7\u30FC\u30BF\u4EA4\u63DB\u5F62\u5F0F\u3068\u3057\u3066\
  \u5E45\u5E83\u304F\u53D7\u3051\u5165\u308C\u3089\u308C\u3066\u3044\u308B\u305F\u3081\
  \u3001\u7570\u306A\u308B\u30B7\u30B9\u30C6\u30E0\u9593\u3067\u306E\u30C7\u30FC\u30BF\
  \u306E\u30A4\u30F3\u30DD\u30FC\u30C8\u3001\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u3001\
  \u304A\u3088\u3073\u7BA1\u7406\u306B\u3053\u308C\u3092\u5229\u7528\u3057\u307E\u3059\
  \u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 何となぜ?

CSV（カンマ区切り値）ファイルを扱うことは、テキストの各行が表の行を表し、個々の列がカンマで区切られる簡単なテキスト形式に格納されたデータの処理および操作についてです。プログラマーは、CSVが軽量で人が読めるデータ交換形式として幅広く受け入れられているため、異なるシステム間でのデータのインポート、エクスポート、および管理にこれを利用します。

## 方法:

### C++標準ライブラリを使用したCSVファイルの読み込み:

```cpp
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream file("data.csv");
    std::string line;
    
    while (std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::string cell;
        std::vector<std::string> parsedRow;
        
        while (std::getline(lineStream, cell, ',')) {
            parsedRow.push_back(cell);
        }
        
        // ここでparsedRowを処理
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### CSVファイルへの書き込み:

```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"名前", "年齢", "都市"},
        {"John Doe", "29", "New York"},
        {"Jane Smith", "34", "Los Angeles"}
    };
    
    for(const auto& row : data) {
        for (size_t i = 0; i < row.size(); i++) {
            file << row[i];
            if (i < row.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### サードパーティのライブラリを使用する: `csv2`:

C++標準ライブラリはファイルや文字列を扱うための基本ツールを提供していますが、サードパーティライブラリを活用することでCSVの処理を簡素化できます。そのようなライブラリの一つが、使いやすさと効率性で知られる`csv2`です。

- インストール: 通常、ConanのようなパッケージマネージャーやGitHubリポジトリから直接インストールされます。

`csv2`を使用したCSVファイルの読み取り例:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // 各セル値を印刷
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

読み取り操作のサンプル出力は、簡単な3列のCSVファイルを仮定して、このようになります:

```
John    29    New York    
Jane    34    Los Angeles
```

これらの例は、C+での基本的なCSV操作をカバーすることを目的としています。大きなファイルの処理や複雑なデータ変換など、より複雑なシナリオに対処するためには、専門のライブラリやツールへのさらなる探求が必要になるかもしれません。
