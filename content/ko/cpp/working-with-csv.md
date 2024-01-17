---
title:                "csv로 작업하기"
html_title:           "C++: csv로 작업하기"
simple_title:         "csv로 작업하기"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
CSV 파일을 다루는 것은 간단하게 말해 데이터를 스프레드시트 형태로 저장하는 방법입니다. 프로그래머들은 일반적으로 CSV 파일을 사용하여 데이터를 효율적으로 저장하고 처리할 수 있기 때문에 이를 사용합니다.

## 하는 방법:
아래 코드 블록 내에 있는 코딩 예시와 샘플 출력을 참고하여 CSV 파일을 처리하는 방법을 배워보세요.

```C++
// CSV 파일 읽기
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>

using namespace std;

int main() {
  // 파일 열기
  ifstream file("data.csv");
  
  // 파일에서 한 줄씩 읽어오기
  string line;
  while (getline(file, line)) {
    // 한 줄을 쉼표로 분리하여 저장하기
    stringstream ss(line);
    string data;
    vector<string> row;

    while (getline(ss, data, ',')) {
      row.push_back(data);
    }

    // 출력하기
    for (string value : row) {
      cout << value << " ";
    }
    cout << endl;
  }
}

// 출력:
// apple banana orange
// 123 456 789 
```

## 깊이있는 정보:
CSV 파일은 1970년대 부터 사용되어온 데이터 저장 및 처리 방식으로, 현재도 많은 프로그래머들이 사용하고 있습니다. CSV 파일 대신에 JSON 파일을 사용할 수도 있지만, CSV 파일은 간단하고 다루기 쉽기 때문에 많은 경우에 더 유용합니다. CSV 파일은 쉽게 읽고 쓸 수 있으며, 데이터 처리 방식도 다양하기 때문에 많은 프로그래머들이 선호하는 파일 형식이 됐습니다. C++에서 CSV 파일을 처리하기 위해 사용되는 주요 라이브러리에는 Boost와 CsvParser 등이 있습니다.

## 참고 자료:
- [C++ Boost 라이브러리](https://www.boost.org/)
- [CsvParser 라이브러리](https://github.com/evandrix/CsvParser)