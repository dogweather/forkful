---
title:                "处理CSV文件"
html_title:           "C++: 处理CSV文件"
simple_title:         "处理CSV文件"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## 什么是 CSV ？为什么程序员需要它？
CSV（Comma-separated values）是一种常见的文件格式，它被用来存储大量简单数据，如电子表格数据或数据库导出。程序员经常使用CSV来处理数据，并将其转换为其他格式。

## 如何操作 CSV：
### 示例一：
```C++
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

// 从CSV文件中读取数据并存储在二维向量中
vector<vector<string>> read_csv(string filename) {
    vector<vector<string>> data; // 存储数据的二维向量
    ifstream file(filename);

    // 检查文件是否存在
    if(file.is_open()) {
        string line;

        // 按行读取文件
        while(getline(file, line)) {
            vector<string> row; // 存储每一行数据的向量
            stringstream ss(line); // 使用stringstream对象将字符串分割为各个字段

            string field;

            // 将每个字段添加到行向量中
            while(getline(ss, field, ',')) {
                row.push_back(field);
            }
            data.push_back(row); // 将行向量添加到数据向量中
        }

        file.close(); // 关闭文件
    }

    return data;
}

int main() {
    // 从example.csv文件中读取数据
    vector<vector<string>> csv_data = read_csv("example.csv");

    // 打印数据
    for(auto row : csv_data) {
        for(auto field : row) {
            cout << field << " ";
        }
        cout << endl;
    }
}
```

### 示例二：
```C++
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
using namespace std;

// 将数据写入CSV文件中
void write_csv(string filename, vector<vector<string>> data) {
    ofstream file(filename);
    
    // 写入文件
    for(auto row : data) {
        for(auto field : row) {
            file << field << ",";
        }
        file << endl;
    }

    file.close(); // 关闭文件
}

int main() {
    // 构建数据向量
    vector<vector<string>> csv_data;
    vector<string> row1 = {"John", "Doe", "38"};
    vector<string> row2 = {"Jane", "Smith", "25"};
    csv_data.push_back(row1);
    csv_data.push_back(row2);

    // 将数据写入new_file.csv文件中
    write_csv("new_file.csv", csv_data);

    return 0;
}
```

输出示例：
```
John Doe 38 
Jane Smith 25
```

## 深入了解 CSV：
### 历史背景：
CSV格式最早在20世纪70年代被创建，旨在提供一种简单的方法来共享电子表格数据。它通过使用逗号作为分隔符来实现，因此得名逗号分隔值（Comma-separated values）。

### 其他替代方案：
除了CSV，还有其他格式可用于存储大量数据，如JSON、XML等。每种格式都有其自身的特点和用途，但CSV仍然是处理简单数据的一种方便的选择。

### 实现细节：
在C++中，可以使用fstream库来读写文件。基本的原理是将文件内容读入到字符串或向量中，并按照某种规则进行分割和处理。

## 参考资料：
https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm
https://www.techopedia.com/definition/29438/comma-separated-values-csv