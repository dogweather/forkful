---
title:                "C++: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

CSV 파일은 컴퓨터 과학에서 매우 중요한 파일 형식입니다. 데이터를 저장하고 공유하는 데에 널리 사용되며, 많은 프로그래머들이 자주 다루게 될 것입니다.

## 방법

우선, 필요한 라이브러리를 포함시키는 것으로 시작합니다. 다음 코드를 사용하여 이 작업을 수행할 수 있습니다:

```C++
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
```

그런 다음 CSV 파일을 읽어서 데이터를 추출하는 방법을 다룰 것입니다. 다음 코드를 사용하여 파일을 열고, 데이터를 읽어오고, 필요한 형식으로 변환할 수 있습니다:

```C++
// CSV 파일 열기
std::ifstream file("data.csv");
// 데이터 저장을 위한 벡터 생성
std::vector<std::vector<std::string>> data;
// 파일에서 데이터 읽기
std::string line;
while (std::getline(file, line)) {
  std::stringstream ss(line);
  std::vector<std::string> row;
  std::string value;
  // 쉼표를 기준으로 데이터 추출
  while (std::getline(ss, value, ',')) {
    row.push_back(value);
  }
  // 벡터에 데이터 추가
  data.push_back(row);
}
// 데이터 출력
for (std::vector<std::string> row : data) {
  for (std::string value : row) {
    // 출력 형식에 맞게 수정
    std::cout << value << " ";
  }
  std::cout << std::endl;
}
```

예시 파일(data.csv):

```
Alice,20,Female
Bob,25,Male
```

출력 결과:

```
Alice 20 Female
Bob 25 Male
```

## 딥 다이브

CSV 파일은 쉼표(,)로 구분된 데이터를 포함하는 텍스트 파일입니다. 이러한 파일들은 보통 표 형식으로 데이터를 구성하기 때문에 스프레드시트 프로그램에서도 쉽게 열어볼 수 있습니다. 또한 프로그래밍에서 이러한 데이터를 파싱하고 다룰 수 있기 때문에 매우 유용한 파일 형식입니다. CSV 파일은 헤더(열의 라벨)와 각 열의 데이터가 구분된 쉼표로 이루어져 있습니다.

## 관련 자료

- [CSV 파일 적용하기](https://www.delftstack.com/ko/tutorial/c-plus-plus-tutorial/how-to-read-csv-file-in-cplusplus/)
- [프로그래밍에서 CSV 파일 다루기](https://www.tutorialspoint.com/cplusplus-program-to-read-a-csv-file)
- [C++에서 간단한 CSV 파싱 방법](https://gist.github.com/kramimus/4e2e40f2d4041bfd3cef)

## 더 알아보기

- [CSV 파일 형식](https://en.wikipedia.org/wiki/Comma-separated_values)
- [CSV vs. TSV](https://www.rev.com/blog/cs-101-csv-vs-tsv-differences-between-comma-and-tab-delimited-files)