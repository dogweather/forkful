---
title:                "텍스트 파일 읽기"
html_title:           "Javascript: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 읽는 것에 대해 왜 관심을 갖게 되는지 최대 2문장으로 설명합니다.

텍스트 파일을 읽는 것은 자바스크립트 프로그래밍에서 필수적인 기술입니다. 텍스트 파일을 읽을 수 있게 되면 프로그램에서 파일에 저장된 데이터를 활용할 수 있어 프로그램을 더욱 유연하고 다양하게 만들 수 있습니다. 또한 웹 서비스나 앱 개발에서도 많이 활용되는 기술이므로 반드시 알아두어야 합니다.

## 방법
코드 블록인 "```Javascript ... ```" 안에 코딩 예제와 출력 결과를 포함합니다.

```Javascript
// 텍스트 파일을 읽기 위해 내장 모듈 fs를 불러옵니다.
const fs = require('fs');

// fs.readFile() 메소드를 사용하여 파일을 읽습니다.
fs.readFile('sample.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});

// sample.txt 파일의 내용을 출력합니다.
// Hello, world!
```

## 깊게 파헤치기
텍스트 파일을 읽는 방법은 다양하지만, 가장 많이 사용되는 방법은 내장 모듈 fs의 readFile() 메소드를 사용하는 것입니다. 이 메소드는 비동기적으로 파일을 읽어올 수 있으며, 파일 읽기가 완료될 때 콜백 함수를 실행하여 결과를 처리합니다. 또한 fs 모듈 외에도 다른 외부 모듈을 이용해 더 다양하게 파일을 읽을 수도 있습니다.

## 관련 자료
"## 관련 자료" (See Also) 라는 마크다운 헤딩 아래에 관련된 링크를 나열합니다.

- [Node.js 파일 시스템 (fs)](https://nodejs.org/dist/v14.15.3/docs/api/fs.html)
- [비동기 처리와 콜백 함수](https://velog.io/@yejinh/JavaScript%EB%B9%84%EB%8F%99%EA%B8%B0%EC%B2%98-%EC%BD%9C%EB%B0%B1%ED%95%A8%EC%88%98-%EC%97%B0%EC%82%B0%EC%A0%95%EB%A6%AC)