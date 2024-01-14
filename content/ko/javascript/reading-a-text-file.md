---
title:    "Javascript: 텍스트 파일 읽기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 이유는 데이터를 분석하거나 사용하기 위해서입니다. 텍스트 파일은 다양한 정보를 담고 있기 때문에 프로그래밍에서 중요한 역할을 합니다.

## 해야할 일

텍스트 파일을 읽는 방법은 다양하지만, 간단한 예제로 알아보겠습니다.

```Javascript
const fs = require('fs'); // 파일 시스템 모듈 불러오기

// readFile 메소드를 이용하여 텍스트 파일 읽기
fs.readFile('sample.txt', 'utf8', function (err, data) {
  if (err) {
    console.log(err); // 오류 발생 시 출력
    return;
  }
  console.log(data);  // 파일 내용 출력
});
```

위 코드는 Node.js를 이용해서 텍스트 파일을 읽는 방법을 보여줍니다. 먼저 파일 시스템 모듈을 불러오고, `readFile` 메소드를 사용하여 텍스트 파일의 내용을 읽은 뒤 출력합니다.

## 더 깊게

텍스트 파일을 읽는 과정에서 발생할 수 있는 오류를 처리하는 방법도 알아보겠습니다.

```Javascript
const fs = require('fs');  // 파일 시스템 모듈 불러오기

// try-catch 문을 이용하여 오류 처리
try {
  const data = fs.readFileSync('sample.txt', 'utf8');
  console.log(data);  // 파일 내용 출력
} catch (err) {
  console.log(err); // 오류 발생 시 출력
}
```

`readFile` 메소드 대신 `readFileSync` 메소드를 사용하면 오류가 발생할 경우 `try-catch` 문을 이용하여 예외처리를 할 수 있습니다.

## 참고자료

- [Node.js 파일 시스템 모듈 문서](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [자바스크립트 예외처리 방법](https://velog.io/@surim014/JavaScript%EC%97%90%EC%84%9C-%EC%98%88%EC%99%B8-%EC%B2%98%EB%A6%AC%EB%B0%A9%EB%B2%95)

## 참조

- [Node.js 파일 시스템 모듈 문서](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [자바스크립트 예외처리 방법](https://velog.io/@surim014/JavaScript%EC%97%90%EC%84%9C-%EC%98%88%EC%99%B8-%EC%B2%98%EB%A6%AC%EB%B0%A9%EB%B2%95)