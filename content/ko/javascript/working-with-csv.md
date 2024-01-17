---
title:                "CSV 작업하기"
html_title:           "Javascript: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

"## 무엇 & 왜?"
CSV란 무엇이며 프로그래머들이 왜 이 작업을 하는지에 대해 두세 문장으로 설명한다.

CSV(Comma Separated Values)는 텍스트 파일 형식의 일종으로, 데이터를 쉼표 또는 다른 구분자로 구분하여 저장하는 방식을 말한다. 프로그래머들은 이러한 형식을 사용하여 데이터를 효율적으로 관리하고 처리하기 위해 CSV를 사용한다.

"## 사용 방법:"
```Javascript
// CSV 데이터 파일 불러오기
const fs = require('fs'); // 파일 시스템 모듈 불러오기
const csv = require('csv-parser'); // CSV 파일 파서 불러오기

// CSV 데이터 파일을 읽어 각 줄의 데이터를 출력하는 함수
fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (row) => {
    console.log(row); // 각 줄의 데이터 출력
  })
  .on('end', () => {
    console.log('CSV 파일 읽기 완료.');
  });
```

위 예제는 Node.js 환경에서 CSV 파일을 읽어 각 줄의 데이터를 출력하는 방법을 보여준다. 파일 시스템 모듈을 사용하여 파일을 읽고, csv-parser 모듈을 사용하여 CSV 파일을 파싱한 후, 각 줄의 데이터를 출력한다.

"## 깊이 있는 내용:"
1. 역사적 맥락: CSV는 1972년 마틴 므슐러(Martin-Michell)와 버클리 프로그래밍 연구실(Berkeley Programming Research)에서 만들어졌다. 해당 연구에서는 개인 컴퓨터나 조그마한 컴퓨터를 사용하는 커뮤니티를 대상으로 데이터를 공유하고 교환할 수 있는 방법을 찾고자 하였다. 그 결과 탄생한 것이 CSV 포맷이다.

2. 대안: CSV 포맷 뿐만 아니라 다양한 데이터 파일 형식이 존재한다. JSON, XML 등 다른 형식을 사용하거나 DB 관리 시스템을 이용할 수도 있다. 하지만 CSV는 간단하고 깔끔한 형식으로 데이터를 저장하며, 다른 시스템과 데이터를 주고받을 때 호환성이 높기 때문에 여전히 많이 사용되고 있다.

3. 구현 세부 사항: CSV 파일은 간단한 형식이지만 다양한 변형들이 존재한다. 빈 갑이 있는지, 갑 사이에 따옴표를 사용하는지 등 다양한 경우를 고려하여 적절한 파싱 방법을 선택해야 한다. 또한 한글과 같은 다국어 문자를 다룰 때 인코딩 문제가 발생할 수 있으므로 적절한 인코딩 설정도 필요하다.

"## 관련 자료:"
CSV 관련 다양한 자료를 확인할 수 있는 링크를 제공한다.
- CSV 문서: https://tools.ietf.org/html/rfc4180
- Node.js CSV 라이브러리: https://csv.js.org/
- CSV 관련 다양한 라이브러리들: https://github.com/awslabs/aws-athena-query-federation/blob/master/federation-sdk/types/csv/csv.js