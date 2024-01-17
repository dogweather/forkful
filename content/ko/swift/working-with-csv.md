---
title:                "Csv 작업"
html_title:           "Swift: Csv 작업"
simple_title:         "Csv 작업"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## 뭘 & 왜?

CSV 작업이란 무엇인지, 그리고 프로그래머들이 그것을 왜 하는지에 대해서 설명해보겠습니다.

먼저, CSV란 Comma-Separated Values의 약자로, 쉼표로 구분된 데이터를 의미합니다. 즉, 엑셀 등 스프레드시트 프로그램에서는 각 셀이 쉼표로 구분되어 데이터를 저장하는데, 이러한 데이터를 작업하는 것이 CSV 작업이죠.

그리고 프로그래머들은 CSV 파일을 다루는 것이 유용한 이유가 있습니다. 예를 들어, 다수의 데이터를 다룰 때 많은 시간과 노력을 절약할 수 있고, 데이터베이스를 다룰 때 보다 유용하게 사용될 수 있습니다.

## 사용 방법:

```Swift
// CSV 파일에서 데이터 읽어오기
if let csvURL = Bundle.main.url(forResource: "data", withExtension: "csv") {
	if let csvData = try? String(contentsOf: csvURL) {
		// 데이터를 줄 단위로 구분
		let csvRows = csvData.components(separatedBy: "\n")
		
		// 각 줄에서 데이터를 쉼표로 구분
		for row in csvRows {
			let rowData = row.components(separatedBy: ",")
			print(rowData)
		}
	}
}
```
위 코드는 CSV 파일에서 데이터를 읽어오는 간단한 예시입니다. 먼저, `Bundle` 클래스를 이용해 파일의 경로를 가져오고, `String` 클래스의 `components(separatedBy:)` 메서드를 사용하여 줄과 데이터를 구분한 뒤, `print` 함수를 이용해 데이터를 출력합니다.

```Swift
// CSV 파일에 데이터 저장
var csvData = "Name, Age, Score\n"
csvData += "John, 24, 85\n"
csvData += "Jane, 28, 92\n"
csvData += "Tom, 22, 79"

if let csvURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first?.appendingPathComponent("data.csv") {
	try? csvData.write(to: csvURL, atomically: true, encoding: .utf8)
}
```
위 코드는 CSV 파일에 데이터를 저장하는 예제입니다. 가장 먼저, 문자열 변수 `csvData`에 데이터를 쉼표로 구분하여 저장하고, `FileManager` 클래스를 이용해 파일 경로를 가져온 뒤, `String` 클래스의 `write(to:atomically:encoding:)` 메서드를 이용해 파일을 생성하고 데이터를 저장합니다.

## 깊이 들어가기:

CSV는 1970년대에 발명된 데이터 관리 방법 중 하나로, 현재 많은 기업에서 사용되고 있습니다. 또한, CSV 파일은 다른 파일 형식보다 더 작은 크기로 저장되기 때문에, 많은 데이터를 저장할 때 유용합니다.

하지만, CSV 파일은 데이터 무결성을 보장하지 않기 때문에, 데이터 저장에 사용되는 다른 형식들과 비교할 때 장단점이 있습니다. 따라서, 데이터를 다룰 때 CSV 파일에 대한 이해가 필요합니다.

## 관련 자료:

- [Swift CSV Parser](https://github.com/yaslab/CSV.swift)
- [How to Read and Write CSV Files](https://www.hackingwithswift.com/example-code/system/how-to-read-and-write-csv-files)
- [The History of CSV Files](https://www.science.co.il/programming/History-of-CSV.php)