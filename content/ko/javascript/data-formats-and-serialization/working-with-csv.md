---
title:                "CSV와 함께 작업하기"
date:                  2024-02-03T19:20:28.528101-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV와 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
자바스크립트에서 CSV(쉼표로 구분된 값들)를 처리하는 것은 외부 소스로부터 표 형식 데이터를 가져오거나 다른 프로그램에서 사용할 데이터를 내보내기 위해 CSV 파일을 파싱하거나 생성하는 것을 말합니다. 프로그래머들이 이를 수행하는 이유는 애플리케이션, 데이터베이스, 시스템 간에 복잡한 형식인 JSON 보다는 간결하고 가벼운 데이터 교환을 가능하게 하기 때문입니다.

## 방법:
자바스크립트는 JSON과 같은 내장 CSV 파싱이나 문자열화 기능이 없습니다. 그러나 단순한 작업의 경우 원시 자바스크립트를 사용하거나 `PapaParse`와 같은 강력한 라이브러리를 활용하여 더 복잡한 시나리오에서 CSV 데이터를 쉽게 관리할 수 있습니다.

### 원시 자바스크립트로 기본 파싱하기
단순한 CSV 문자열을 객체의 배열로 파싱하기:

```javascript
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const lines = csv.split("\n");
  const result = [];
  const headers = lines[0].split(",");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split(",");
    
    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }
    result.push(obj);
  }
  
  return result;
}

console.log(parseCSV(csv));
```
출력:

```
[
  { name: 'John', age: '23', city: 'New York' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### 원시 자바스크립트로 CSV 기본 생성하기
객체의 배열을 CSV 문자열로 변환하기:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(row => 
    Object.values(row).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

출력:

```
John,23,New York
Jane,28,Los Angeles
```

### 복잡한 CSV 작업을 위해 PapaParse 사용하기
더 복잡한 시나리오의 경우, `PapaParse`는 스트림, 워커, 큰 파일 처리 옵션을 가지고 있는 강력한 라이브러리로 CSV 파일을 파싱하고 문자열화하기에 적합합니다.

PapaParse로 CSV 파일이나 문자열 파싱하기:

```javascript
// 프로젝트에 PapaParse를 추가한 후
const Papa = require('papaparse');
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Parsed:", results.data);
  }
});
```

생성됨:

```
Parsed: [
  ["name", "age", "city"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

PapaParse로 배열을 CSV 문자열로 문자열화하기:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

생성:

```
name,age,city
John,23,New York
Jane,28,Los Angeles
```

이 예시들은 기본적이고 고급 CSV 처리를 자바스크립트에서 보여주어 웹 애플리케이션과 그 너머에서 데이터 교환을 용이하게 합니다.