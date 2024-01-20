---
title:                "处理 CSV 文件"
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
处理CSV文件就是操作存储在逗号分隔值（CSV）格式中的数据。程序员因为CSV的通用性、简单性和跨平台特性而经常使用它来交换数据。

## How to: (如何操作：)
```C++
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

// CSV解析函数
std::vector<std::vector<std::string>> parseCSV(const std::string& filename) {
    std::vector<std::vector<std::string>> data;
    std::ifstream file(filename);
    std::string line;
    
    // 读取每一行
    while (getline(file, line)) {
        std::stringstream linestream(line);
        std::string cell;
        std::vector<std::string> parsedLine;
        
        // 读取每个单元格
        while (getline(linestream, cell, ',')) {
            parsedLine.push_back(cell);
        }
        
        data.push_back(parsedLine);
    }
    
    return data;
}

int main() {
    std::vector<std::vector<std::string>> csvData = parseCSV("example.csv");
    
    // 打印CSV文件内容
    for (const auto& row : csvData) {
        for (const auto& col : row) {
            std::cout << col << ' ';
        }
        std::cout << '\n';
    }
    
    return 0;
}
```
输出样例：
```
id name age 
1 Alice 30 
2 Bob 25 
```

## Deep Dive (深入了解)
CSV格式源自早期计算机科学，简化了表格数据的交互方式。尽管JSON和XML等格式也能提供类似的数据交换功能，CSV因其简洁和创建/解析上的效率保持流行。C++语言处理CSV通常需要自行解析文本或使用第三方库如`Boost`。

## See Also (另请参阅)
- C++文档: https://cplusplus.com/
- Boost库: https://www.boost.org/
- 视频教程（用C++操作CSV）: https://www.youtube.com/watch?v=_O2zklPLBsU