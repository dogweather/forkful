---
title:                "Swift: CSV와 함께 작업하기"
simple_title:         "CSV와 함께 작업하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜
CSV 파일을 다루는 것에 관심이 있을까요? 컴퓨터 과학 학생이나 개발자라면 데이터 분석 또는 앱 개발 등의 목적으로 CSV 파일을 사용할 수 있습니다.

## 방법
이제 한 번 실제 Swift 코드로 CSV 파일을 다루는 방법을 알아보겠습니다.

### CSV 파일 읽기
```Swift
import Foundation

// 파일 경로 설정
let filePath = "/Users/myuser/Documents/data.csv"

do {
    // 파일을 문자열로 읽어옴
    let data = try String(contentsOfFile: filePath)
    
    // 라인별로 나눠서 배열로 저장
    let rows = data.components(separatedBy: "\n")
    
    // 각 라인을 콤마로 분리하여 2차원 배열로 저장
    var table : [[String]] = []
    for row in rows {
        table.append(row.components(separatedBy: ","))
    }
    
    // 출력
    print(table)
    
} catch {
    // 파일을 읽지 못한 경우 에러 처리
    print(error)
}
```

### CSV 파일 쓰기
```Swift
import Foundation

// 저장할 데이터 배열 생성
let fruits = ["Apple", "Banana", "Cherry", "Durian"]

// 파일 경로 설정
let filePath = "/Users/myuser/Documents/fruit.csv"

do {
    // 데이터 배열을 쉼표로 나누어 문자열로 저장
    let data = fruits.joined(separator: ",")
    
    // 파일에 쓰기
    try data.write(toFile: filePath, atomically: true, encoding: .utf8)
    
    // 파일이 성공적으로 저장되었다는 메시지 출력
    print("CSV 파일을 성공적으로 저장하였습니다.")
    
} catch {
    // 파일을 쓰지 못한 경우 에러 처리
    print(error)
}
```

## 깊은 곳 탐색
CSV 파일은 쉽게 읽고 쓸 수 있는 형식이지만, 파일 크기가 커지면 읽어오는 데 오랜 시간이 걸릴 수 있습니다. 이 경우 메모리를 효율적으로 사용하기 위해 `StreamReader` 클래스를 사용할 수 있습니다. 또한 CSV 파일에는 다양한 형식이 존재할 수 있기 때문에 데이터를 적절하게 처리하기 위해 정규표현식을 사용하는 것도 좋은 방법입니다.

## 참고 자료
- [Swift Foundation](https://developer.apple.com/documentation/foundation)
- [Regular Expressions in Swift](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial)
- [StreamReader Class](https://gist.github.com/blanu/295bebcbb520cb3cd239)