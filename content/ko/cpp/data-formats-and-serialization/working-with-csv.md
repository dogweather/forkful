---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:19.175186-07:00
description: ''
lastmod: '2024-04-05T22:00:08.482104-06:00'
model: gpt-4-0125-preview
summary: ''
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

## 방법:


### C++ 표준 라이브러리를 사용하여 CSV 파일 읽기:
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
        
        // 여기에서 parsedRow를 처리
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### CSV 파일 쓰기:
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

### 서드파티 라이브러리 사용하기: `csv2`:
C++ 표준 라이브러리가 파일 및 문자열 작업을 위한 기본 도구를 제공하는 동안, 서드파티 라이브러리를 활용하면 CSV 처리를 단순화할 수 있습니다. 이런 라이브러리 중 하나가 사용의 용이성과 효율성으로 알려진 `csv2` 입니다.

- 설치: 일반적으로 Conan 또는 GitHub 리포지토리에서 직접 설치합니다.

`csv2`를 사용하여 CSV 파일 읽기 예:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // 각 셀 값 출력
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

읽기 작업에 대한 샘플 출력은 아래와 같을 수 있습니다 (간단한 세 열짜리 CSV 파일을 가정):

```
John    29    New York    
Jane    34    Los Angeles
```

이 예제들은 C++에서 기본적인 CSV 작업을 다루기 위한 것입니다. 대용량 파일을 다루거나 복잡한 데이터 변환과 같은 더 복잡한 시나리오의 경우, 전문 라이브러리나 도구로 더 깊이 탐색할 필요가 있을 수 있습니다.
