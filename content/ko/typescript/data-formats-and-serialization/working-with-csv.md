---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:28.133678-07:00
description: "\uBC29\uBC95: TypeScript\uC5D0\uC11C\uB294 `csv-parser` \uAC19\uC740\
  \ \uC11C\uB4DC \uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD65C\uC6A9\uD558\
  \uAC70\uB098, \uB124\uC774\uD2F0\uBE0C \uCF54\uB4DC\uB97C \uD1B5\uD574 CSV \uD30C\
  \uC77C\uC744 \uB2E4\uB8F0 \uC218 \uC788\uC2B5\uB2C8\uB2E4. `csv-parser`\uB294 \uC77D\
  \uAE30\uC6A9\uC774\uACE0, `csv-writer`\uB294 \uC4F0\uAE30\uC6A9\uC785\uB2C8\uB2E4\
  . \uBA3C\uC800 npm\uC744 \uD1B5\uD574 `csv-parser`\uB97C\u2026"
lastmod: '2024-03-13T22:44:54.885696-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\uC5D0\uC11C\uB294 `csv-parser` \uAC19\uC740 \uC11C\uB4DC \uD30C\
  \uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD65C\uC6A9\uD558\uAC70\uB098, \uB124\
  \uC774\uD2F0\uBE0C \uCF54\uB4DC\uB97C \uD1B5\uD574 CSV \uD30C\uC77C\uC744 \uB2E4\
  \uB8F0 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

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
