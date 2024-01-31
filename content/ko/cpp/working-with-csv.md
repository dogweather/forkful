---
title:                "CSV 파일 다루기"
date:                  2024-01-19
simple_title:         "CSV 파일 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
CSV(Comma-Separated Values)는 자료를 쉼표로 구분해 저장하는 파일 형식이에요. 프로그래머들이 데이터를 쉽고 호환성 높게 주고받을 수 있어서 자주 쓰지요.

## How to: (사용 방법)
```C++
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

// CSV 한 줄 읽기
std::vector<std::string> readCSVRow(const std::string &row) {
    std::stringstream ss(row);
    std::vector<std::string> result;
    std::string cell;

    while (std::getline(ss, cell, ',')) {
        result.push_back(cell);
    }

    return result;
}

// CSV 파일 읽기
std::vector<std::vector<std::string>> readCSVFile(const std::string &filename) {
    std::vector<std::vector<std::string>> data;
    std::ifstream file(filename);
    std::string row;

    while (std::getline(file, row)) {
        data.push_back(readCSVRow(row));
    }

    return data;
}

// CSV 파일 쓰기
void writeCSVFile(const std::string &filename, const std::vector<std::vector<std::string>> &data) {
    std::ofstream file(filename);

    for (const auto &row : data) {
        for (size_t i = 0; i < row.size(); ++i) {
            file << row[i];
            if (i < row.size() - 1) file << ',';
        }
        file << '\n';
    }
}

int main() {
    // 파일 읽기
    std::vector<std::vector<std::string>> data = readCSVFile("example.csv");

    // 읽은 데이터 출력
    for (const auto &row : data) {
        for (const auto &cell : row) {
            std::cout << cell << " ";
        }
        std::cout << std::endl;
    }

    // 파일 쓰기
    data.push_back({"새", "줄"});
    writeCSVFile("example.csv", data);
    
    return 0;
}
```

다음은 간단한 CSV 파일에서 데이터를 읽고, 추가한 뒤 파일에 다시 쓰는 코드 예제입니다.

## Deep Dive (심층 분석)
CSV는 유닉스 시대부터 시작해 지금까지 쓰이고 있는 오래된 형식입니다. Excel 같은 툴이 CSV를 쉽게 다룰 수 있어 널리 퍼졌죠. JSON이나 XML과 같은 현대적인 양식도 있지만, CSV는 단순함과 가독성에서 강점이 있어요. C++에서는 `<fstream>` 라이브러리로 파일을 다루는데, CSV 형식은 파싱하기 쉽기 때문에 문자열 조작 함수와 효율적으로 결합할 수 있습니다.

## See Also (참고자료)
- CSV 관련 C++ 라이브러리: [https://github.com/ben-strasser/fast-cpp-csv-parser](https://github.com/ben-strasser/fast-cpp-csv-parser)
- C++ 파일 입출력 가이드: [http://www.cplusplus.com/doc/tutorial/files/](http://www.cplusplus.com/doc/tutorial/files/)
- C++ 표준 라이브러리 레퍼런스: [https://en.cppreference.com/w/](https://en.cppreference.com/w/)
