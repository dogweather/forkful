---
title:                "CSV와 함께 작업하기"
date:                  2024-02-03T19:21:28.133678-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV와 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV(콤마로 구분된 값)를 다루는 작업은 CSV 파일을 읽고 쓰는 과정을 포함합니다. CSV는 그 단순성과 다양한 플랫폼 및 언어에서의 폭넓은 지원 때문에 널리 사용되는 데이터 교환 형식입니다. 프로그래머들은 애플리케이션이나 데이터베이스, 서비스로부터 데이터를 가져오거나 내보내기 위해 CSV 파일을 다루며, 이를 통해 데이터 조작 및 공유가 쉽게 이루어집니다.

## 방법:

TypeScript에서는 `csv-parser` 같은 서드 파티 라이브러리를 활용하거나, 네이티브 코드를 통해 CSV 파일을 다룰 수 있습니다. `csv-parser`는 읽기용이고, `csv-writer`는 쓰기용입니다.

### `csv-parser`로 CSV 읽기

먼저 npm을 통해 `csv-parser`를 설치하세요:

```
npm install csv-parser
```

그런 다음, 다음과 같이 CSV 파일을 읽습니다:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // 출력: 각각 CSV의 한 행을 나타내는 객체의 배열
  });
```

`data.csv`가 다음을 포함한다고 가정합니다:

```
name,age
Alice,30
Bob,25
```

출력은 다음과 같습니다:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### `csv-writer`로 CSV 쓰기

CSV 파일에 쓰기 위해서는, 먼저 `csv-writer`를 설치하세요:

```
npm install csv-writer
```

그런 다음, 다음과 같이 사용합니다:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NAME'},
    {id: 'age', title: 'AGE'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('The CSV file was written successfully'));
```

이 코드는 `out.csv`에 다음을 씁니다:

```
NAME,AGE
Alice,30
Bob,25
```

이 예제들은 데이터 분석을 위해 데이터를 읽거나 애플리케이션 데이터를 외부에 영속화하기 위해, TypeScript 프로젝트에서 CSV 처리를 효율적으로 통합하는 방법을 보여줍니다.
