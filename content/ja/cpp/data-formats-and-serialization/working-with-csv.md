---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:10.082931-07:00
description: "\u65B9\u6CD5: ."
lastmod: '2024-03-13T22:44:42.583980-06:00'
model: gpt-4-0125-preview
summary: ''
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

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
