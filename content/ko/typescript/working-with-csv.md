---
title:                "CSV 작업"
html_title:           "TypeScript: CSV 작업"
simple_title:         "CSV 작업"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜
사람들이 CSV 파일을 다루는 작업에 참여하는 이유는 무엇일까요? 그것은 파일 형식이 널리 사용되고 있는 간단하고 유용한 방법이기 때문입니다. 그것들을 다루는 것은 뛰어난 솔루션을 제공하기에 매우 중요합니다.

## 이건 어떻게?
```TypeScript
import fs from 'fs';
import csv from 'csv-parser';

// CSV 파일 읽기
const readCSVFile = (filePath: string): void => {
  fs.createReadStream(filePath)
    .pipe(csv())
    .on('data', (data) => {
      console.log(data);
    })
    .on('end', () => {
      console.log('CSV 파일 읽기 완료!');
    });
};

// CSV 파일 쓰기
const writeCSVFile = (filePath: string, data: any[]): void => {
  csv.write(
    data,
    { headers: true },
    (err, output) => {
      if (err) throw err;
      fs.writeFileSync(filePath, output);
      console.log('CSV 파일 쓰기 완료!');
    });
};

// CSV 파일 수정
const updateCSVFile = (filePath: string, newData: any[]): void => {
  const rows: any[] = [];
  const parser = csv.parse({ headers: true });
  fs.createReadStream(filePath)
    .pipe(parser)
    .on('data', (data) => {
      // 기존 데이터 행에 새로운 데이터 열 추가
      newData.forEach((item) => {
        if (data.name === item.name) {
          data.job = item.job;
        }
      });
      rows.push(data);
    })
    // 새로운 데이터로 CSV 파일 덮어쓰기
    .on('end', () => {
      csv.write(
        rows,
        { headers: true },
        (err, output) => {
          if (err) throw err;
          fs.writeFileSync(filePath, output);
          console.log('CSV 파일 수정 완료!');
        }
      );
    });
};

// 함수 호출
const filePath: string = './data.csv';
const data: any[] = [
  { name: 'James', job: 'Developer' },
  { name: 'Emily', job: 'Designer' }
];
// 새로운 CSV 파일 내용
// name, job
// James, Developer
// Emily, Designer

readCSVFile(filePath);
// { name: 'James', job: 'Developer' }
// { name: 'Emily', job: 'Designer' }
// CSV 파일 읽기 완료!

writeCSVFile(filePath, data);
// CSV 파일 쓰기 완료!
// 파일 내용
// name, job
// James, Developer
// Emily, Designer

updateCSVFile(filePath, data);
// CSV 파일 수정 완료!
// 파일 내용
// name, job
// James, Developer
// Emily, Designer

```

## 깊이있는 탐구
CSV 파일 다루기는 보다 깊게 배울 가치가 있습니다. 예를 들어, 서식을 지정하거나 빈 행 및 열을 처리하는 방법 등 다양한 옵션이 있습니다. 또한 TypeScript의 타입 시스템을 활용하여 CSV 데이터의 타입을 정의하고 유효성 검사를 수행할 수 있습니다.

## 스티븐 아웃월
- [CSV 라이브러리 문서](https://csv.js.org/)
- [Node.js에서 CSV 파일 다루기](https://nodejs.org/api/fs.html#fs_fs_createreadstream_path_options)
- [TypeScript 타입 시스템](https://www.typescriptlang.org/docs/handbook/basic-types.html)