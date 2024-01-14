---
title:                "TypeScript: CSV 파일 작업하기"
simple_title:         "CSV 파일 작업하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜
CSV 파일을 사용하는 작업에 참여해야 할 이유는 매우 간단합니다. CSV 파일은 엑셀 등의 스프레드시트 프로그램에서도 많이 사용되며, 데이터를 변환하고 조작하는 데 매우 유용합니다.

## 방법
CSV 파일을 TypeScript로 다루는 것은 매우 간단합니다. 먼저 `fs` 모듈을 통해 CSV 파일을 열고, `csv-parser` 모듈을 사용하여 데이터를 파싱합니다. 그리고 다양한 메소드를 사용하여 데이터를 조작하고 필요한 포맷으로 변환합니다. 아래는 간단한 예제 코드와 출력 결과입니다.
```TypeScript
import * as fs from "fs";
import * as csv from "csv-parser";

// CSV 파일을 엽니다.
fs.createReadStream("data.csv")
  // 데이터를 파싱합니다.
  .pipe(csv())
  // 로직을 적용합니다.
  .on("data", (row) => {
    // 데이터를 조작하고 로직을 적용합니다.
    ...
  })
  // 결과를 출력합니다.
  .on("end", () => {
    console.log("데이터가 조작되었습니다.");
  });
```

아래는 예제 코드의 출력 결과입니다.
```
>> 데이터가 조작되었습니다.
```

## 깊게 파헤치기
CSV 파일을 조작하는 데에는 다양한 방법과 라이브러리가 있습니다. `csv-parser` 모듈 외에도 `fast-csv` 모듈이나 `csvtojson` 모듈 등도 있으며, 각각의 장단점이 있습니다. 또한 `typescript-csv`와 같은 라이브러리는 TypeScript에서 CSV를 더 쉽게 다룰 수 있도록 도와줍니다. 많은 라이브러리를 살펴보고 프로젝트에 가장 적합한 라이브러리를 찾아 사용하는 것이 중요합니다.

## 더 많은 정보
만약 TypeScript로 CSV를 다루는 방법에 대해 더 깊이 알고 싶다면, 아래 링크들을 참고해주세요.
- [Node.js에서 CSV 파일 다루기 (영어)](https://nodejs.org/en/knowledge/advanced/streams/how-to-use-fs-create-read-stream/)
- [csv-parser GitHub 페이지 (영어)](https://github.com/mafintosh/csv-parser)
- [fast-csv GitHub 페이지 (영어)](https://github.com/C2FO/fast-csv)
- [csvtojson NPM 페이지 (영어)](https://www.npmjs.com/package/csvtojson)
- [typescript-csv NPM 페이지 (영어)](https://www.npmjs.com/package/typescript-csv)

## 더 알아보기