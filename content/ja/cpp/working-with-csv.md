---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"

category:             "C++"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
CSVファイルはデータをカンマで分割したテキストファイル。シンプルで互換性が高いため、プログラマはデータのインポート・エクスポートによく使います。

## How to: (方法)
```cpp
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

int main() {
    std::ifstream file("data.csv");
    std::string line;
    std::vector<std::vector<std::string>> data;

    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string cell;
        std::vector<std::string> row;
        
        while (std::getline(ss, cell, ',')) {
            row.push_back(cell);
        }
        
        data.push_back(row);
    }

    for (const auto& row : data) {
        for (const auto& cell : row) {
            std::cout << cell << " ";
        }
        std::cout << '\n';
    }

    return 0;
}
```
```
Name Age City
John 23 New York
Ana 34 Los Angeles
```

## Deep Dive (深い潜入)
CSVは1972年に登場。JSONやXMLといったフォーマットもあるが、シンプルさがウリ。C++では`<fstream>`を使って容易に読み書き可能。例に示したストリーム処理やパース方法は基本的なテクニック。

## See Also (関連情報)
- [C++ Reference - ifstream](http://www.cplusplus.com/reference/fstream/ifstream/)
- [RFC 4180 - Common Format and MIME Type for Comma-Separated Values (CSV) Files](https://tools.ietf.org/html/rfc4180)
- [Stack Overflow - Reading and writing CSV files with C++](https://stackoverflow.com/questions/tagged/csv+c%2b%2b)
