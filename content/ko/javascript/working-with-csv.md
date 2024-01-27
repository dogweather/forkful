---
title:                "CSV 파일 다루기"
date:                  2024-01-19
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV(Comma-Separated Values) 란 자료를 저장할 때 쉼표로 구분한 텍스트 형식입니다. 프로그래머는 데이터를 손쉽게 내보내고, 다루며, 다른 시스템으로 옮길 수 있게 하기 위해 CSV를 사용합니다.

## How to:
```Javascript
// CSV 문자열을 파싱하여 2차원 배열로 변환하는 예시
function parseCSV(csvString) {
  return csvString.split('\n').map(row => row.split(','));
}

const csvData = `Name,Age,Occupation
Alice,30,Engineer
Bob,35,Designer`;
const parsedData = parseCSV(csvData);

console.log(parsedData);
```
Sample Output:
```
[
  ['Name', 'Age', 'Occupation'],
  ['Alice', '30', 'Engineer'],
  ['Bob', '35', 'Designer']
]
```

```Javascript
// 2차원 배열을 CSV 문자열로 변환하는 예시
function arrayToCSV(dataArray) {
  return dataArray.map(row => row.join(',')).join('\n');
}

const arrayData = [
  ['Name', 'Age', 'Occupation'],
  ['Charlie', '25', 'Teacher'],
  ['David', '40', 'Photographer']
];
const csvOutput = arrayToCSV(arrayData);

console.log(csvOutput);
```
Sample Output:
```
Name,Age,Occupation
Charlie,25,Teacher
David,40,Photographer
```

## Deep Dive
CSV 형식은 1970년대부터 사용되었습니다. 간단한 구조 때문에 다양한 프로그래밍 언어와 플랫폼에서 읽고 쓰기 쉽습니다. JSON이나 XML 같은 현대적 형식들과 비교하면, CSV는 사람이 읽기에는 좀 더 어렵지만, 파싱은 빠르고 파일 크기가 작습니다. 구현 측면에서는 정규식이나 외부 라이브러리를 이용해 CSV 파싱과 생성을 더 효율적으로 할 수 있습니다.

## See Also
- MDN Web Docs에서 다루는 ['Array.prototype.join'](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Array/join)
- MDN Web Docs에서 설명하는 ['String.prototype.split'](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- CSV 파일을 다루는 데 널리 쓰이는 JavaScript 라이브러리 [PapaParse](https://www.papaparse.com/)
