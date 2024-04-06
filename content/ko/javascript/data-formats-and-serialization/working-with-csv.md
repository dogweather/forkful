---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:28.528101-07:00
description: "\uBC29\uBC95: \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uB294 JSON\uACFC\
  \ \uAC19\uC740 \uB0B4\uC7A5 CSV \uD30C\uC2F1\uC774\uB098 \uBB38\uC790\uC5F4\uD654\
  \ \uAE30\uB2A5\uC774 \uC5C6\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098 \uB2E8\uC21C\uD55C\
  \ \uC791\uC5C5\uC758 \uACBD\uC6B0 \uC6D0\uC2DC \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\
  \uB97C \uC0AC\uC6A9\uD558\uAC70\uB098 `PapaParse`\uC640 \uAC19\uC740 \uAC15\uB825\
  \uD55C \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD65C\uC6A9\uD558\uC5EC \uB354 \uBCF5\
  \uC7A1\uD55C \uC2DC\uB098\uB9AC\uC624\uC5D0\uC11C CSV \uB370\uC774\uD130\uB97C \uC27D\
  \uAC8C \uAD00\uB9AC\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-04-05T21:53:57.418562-06:00'
model: gpt-4-0125-preview
summary: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uB294 JSON\uACFC \uAC19\uC740 \uB0B4\
  \uC7A5 CSV \uD30C\uC2F1\uC774\uB098 \uBB38\uC790\uC5F4\uD654 \uAE30\uB2A5\uC774\
  \ \uC5C6\uC2B5\uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

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
