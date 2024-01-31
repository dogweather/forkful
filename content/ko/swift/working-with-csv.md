---
title:                "CSV 파일 다루기"
date:                  2024-01-19
simple_title:         "CSV 파일 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
CSV(쉼표로 구분된 값)는 데이터를 저장하기 위한 단순한 형식입니다. 프로그래머들은 다양한 시스템 간에 데이터를 쉽게 전송하고 분석하기 위해 CSV를 사용합니다.

## How to: (방법)
CSV 파일을 읽고 쓰는 간단한 예제입니다.

```Swift
import Foundation

// 샘플 CSV 데이터
let csvData = """
Name,Age,Occupation
Alice,29,Engineer
Bob,35,Designer
"""

// CSV 데이터를 파싱하는 함수
func parseCSV(data: String) -> [[String]] {
    var result: [[String]] = []
    let rows = data.components(separatedBy: "\n")

    for row in rows {
        let columns = row.components(separatedBy: ",")
        result.append(columns)
    }
    
    return result
}

// CSV 데이터 쓰기 함수
func writeCSV(data: [[String]]) -> String {
    var result = ""
    
    for (index, row) in data.enumerated() {
        let rowString = row.joined(separator: ",")
        result += rowString
        if index < data.count - 1 {
            result += "\n"
        }
    }
    
    return result
}

// CSV 데이터 파싱
let parsedData = parseCSV(data: csvData)
print(parsedData)

// 파싱된 데이터를 다시 CSV 형태로 저장
let csvContent = writeCSV(data: parsedData)
print(csvContent)
```

샘플 출력:
```
[["Name", "Age", "Occupation"], ["Alice", "29", "Engineer"], ["Bob", "35", "Designer"]]
Name,Age,Occupation
Alice,29,Engineer
Bob,35,Designer
```

## Deep Dive (심도 있는 탐구)
CSV는 1970년대부터 사용되고 있습니다. 액셀이나 데이터베이스에서 자주 쓰이는 형식입니다. JSON이나 XML 같은 대안들이 있지만, CSV는 이보다 읽기 쉽고 작성하기 쉬운 장점이 있습니다. Swift에서 CSV 작업 구현은 보통 문자열 분리와 함께 배열 사용에 의존합니다.

## See Also (참고 자료)
- [Apple's Swift Documentation](https://developer.apple.com/documentation/swift)
- [Swift의 오픈소스 CSV 파서, CSV.swift](https://github.com/yaslab/CSV.swift)
