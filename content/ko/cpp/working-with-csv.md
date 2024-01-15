---
title:                "csv 파일 다루기"
html_title:           "C++: csv 파일 다루기"
simple_title:         "csv 파일 다루기"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

CSV 파일을 다루는데 관심을 가지는 이유는 여러분이 프로그래밍을 배우는데 있어서 매우 중요한 스킬이기 때문입니다. CSV 파일은 데이터를 쉽게 저장하고 조작할 수 있는 형식이기 때문에 데이터 분석, 웹 개발 등 다양한 분야에서 사용되고 있습니다.

## 방법

먼저, CSV 파일을 다루는 라이브러리인 `csv.h`를 다운로드해야 합니다. 그 후, 불러올 CSV 파일의 경로를 지정해 `csv::CSVReader` 객체를 생성하고 `read_row()` 메소드를 사용하여 데이터를 읽어올 수 있습니다.

```C++
#include <iostream>
#include "csv.h"

int main() {
    // CSV 파일 경로 지정
    csv::CSVReader reader("data.csv");

    // 첫 번째 행 데이터 읽어오기
    csv::CSVRow row = reader.read_row();
    
    // 첫 번째 행 출력
    std::cout << "첫 번째 행 데이터: " << row[0] << " " << row[1] << " " << row[2] << std::endl;

    return 0;
}
```

**출력:**

```
첫 번째 행 데이터: John Smith 35
```

더 많은 정보를 얻고 싶다면 `iterator`를 사용하여 모든 데이터를 순회하며 읽어올 수 있습니다. 또한 `CSVWriter` 클래스를 사용하여 새로운 CSV 파일을 생성하고 데이터를 입력할 수도 있습니다.

## 심층 탐구

CSV 파일은 쉽게 읽고 쓸 수 있지만, 데이터 유형이 복잡하고 포맷이 다양할 경우 문제가 발생할 수 있습니다. 이 때 `csv.h`는 간편하지만 다양한 설정 옵션을 제공하므로 이러한 문제를 해결할 수 있습니다. 또한 CSV 파일을 파싱하거나 다른 데이터 유형으로 변환하는 기능도 제공합니다.

## 참고

- [csv.h documentation](https://github.com/vincentlaucsb/csv-parser)
- [Using CSV files in C++](https://www.geeksforgeeks.org/using-csv-files-c-cpp/)
- [Learn C++ by making a data table](https://www.youtube.com/watch?v=iZ5HBb4LKdQ)