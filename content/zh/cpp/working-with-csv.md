---
title:                "处理CSV文件"
aliases:
- zh/cpp/working-with-csv.md
date:                  2024-02-03T19:19:03.313341-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理CSV文件"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

# 什么 & 为什么？

处理 CSV（逗号分隔值）文件涉及到处理和操作以简单文本格式存储的数据，其中文本的每一行代表表中的一行，而逗号则用来分隔各个列。程序员利用这种方式进行数据的导入、导出和跨不同系统的管理，因为 CSV 作为一种轻量级、可读性强的数据交换格式，已被广泛接受。

## 如何操作：

### 使用 C++ 标准库读取 CSV 文件：

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
        
        // 在此处处理 parsedRow
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### 写入 CSV 文件：

```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Name", "Age", "City"},
        {"John Doe", "29", "New York"},
        {"Jane Smith", "34", "Los Angeles"}
    };
    
    for (const auto& row : data) {
        for (size_t i = 0; i < row.size(); i++) {
            file << row[i];
            if (i < row.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### 使用第三方库：`csv2`：

虽然 C++ 标准库提供了操作文件和字符串的基本工具，但利用第三方库可以简化 CSV 处理工作。`csv2` 就是这样一种库，以其易用性和效率著称。

- 安装：通常通过包管理器如 Conan 安装，或直接从其 GitHub 仓库下载。

使用 `csv2` 读取 CSV 文件的示例：

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // 打印每个单元格的值
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

读取操作的示例输出可能如下（假设是一个简单的三列 CSV 文件）：

```
John    29    New York    
Jane    34    Los Angeles
```

这些示例旨在覆盖 C++ 中基本的 CSV 操作。对于处理大文件或复杂数据转换等更复杂的情景，可能需要进一步探索专门的库或工具。
