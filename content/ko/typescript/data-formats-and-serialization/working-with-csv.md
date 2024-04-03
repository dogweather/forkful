---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:28.133678-07:00
description: "CSV(\uCF64\uB9C8\uB85C \uAD6C\uBD84\uB41C \uAC12)\uB97C \uB2E4\uB8E8\
  \uB294 \uC791\uC5C5\uC740 CSV \uD30C\uC77C\uC744 \uC77D\uACE0 \uC4F0\uB294 \uACFC\
  \uC815\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. CSV\uB294 \uADF8 \uB2E8\uC21C\uC131\
  \uACFC \uB2E4\uC591\uD55C \uD50C\uB7AB\uD3FC \uBC0F \uC5B8\uC5B4\uC5D0\uC11C\uC758\
  \ \uD3ED\uB113\uC740 \uC9C0\uC6D0 \uB54C\uBB38\uC5D0 \uB110\uB9AC \uC0AC\uC6A9\uB418\
  \uB294 \uB370\uC774\uD130 \uAD50\uD658 \uD615\uC2DD\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC774\uB098\
  \ \uB370\uC774\uD130\uBCA0\uC774\uC2A4, \uC11C\uBE44\uC2A4\uB85C\uBD80\uD130 \uB370\
  \uC774\uD130\uB97C \uAC00\uC838\uC624\uAC70\uB098\u2026"
lastmod: '2024-03-13T22:44:54.885696-06:00'
model: gpt-4-0125-preview
summary: "CSV(\uCF64\uB9C8\uB85C \uAD6C\uBD84\uB41C \uAC12)\uB97C \uB2E4\uB8E8\uB294\
  \ \uC791\uC5C5\uC740 CSV \uD30C\uC77C\uC744 \uC77D\uACE0 \uC4F0\uB294 \uACFC\uC815\
  \uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4."
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
